{-# LANGUAGE RecordWildCards #-}

module NLP.Nerf
( train
) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Lazy (forM)
import Control.Arrow (second)
import Data.Char (toLower)
import Data.Maybe (mapMaybe)
import System.IO (hSetBuffering, stdout, BufferMode (..))
import qualified Data.Vector as V
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Control.Monad.Ox
import Numeric.SGD (SgdArgs (..), Para, sgd, sgdM, sgd)
import qualified Control.Monad.Ox.String as Ox

import Data.Named.Tree (mapForest)
import Text.Named.Enamex (parseEnamex)
import qualified Data.Named.Tree as Tr
import qualified Data.Named.IOB as IOB

import qualified Data.CRF.Core as CRF
import qualified Data.CRF.Codec as CRF
import qualified Data.CRF.Labeled as CRF
import qualified Data.CRF.Model as CRF
import qualified Data.CRF.FeatSel.Present as CRF
import qualified Data.CRF.Gradient as CRF
import qualified Data.CRF.Inference as CRF

type Word = String
type NE = String
type Ob = ([Int], String)
type Lb = IOB.Label String

schema :: V.Vector Word -> Int -> Ox Word String ()
schema sent = \k -> do
    mapM_ (save . lowOrth)          [k - 1, k]
    -- mapM_ (save . upOnlyOrth)    [k - 1, k]
    mapM_ lowLemma                  [k - 1, k]
    mapM_ (save . shape)            [k - 1, k]
    mapM_ (save . packedShape)      [k - 1, k]
    -- etc.
  where
    at              = atWith sent
    orth            = (id `at`)
    lowOrth i       = map toLower <$> orth i
    shape i         = Ox.shape <$> orth i
    packedShape i   = Ox.pack <$> shape i

    lowLemma i = group $ do
        mapM_ (save . lowPrefix i) [0, -1, -2, -3]
        mapM_ (save . lowSuffix i) [0, -1, -2, -3]

    lowPrefix i j = Ox.prefix j =<< lowOrth i
    lowSuffix i j = Ox.suffix j =<< lowOrth i

-- | Train the CRF model
train
    :: SgdArgs                      -- ^ Args for SGD
    -> FilePath                     -- ^ Train data (ENAMEX)
    -> Maybe FilePath               -- ^ Maybe eval data (ENAMEX)
    -> IO (CRF.Codec Ob Lb, CRF.Model)    -- ^ Resulting codec and model
train sgdArgs trainPath evalPathM = do
    hSetBuffering stdout NoBuffering
    (codec, trainData)
        <- second V.fromList . CRF.encodeData
        <$> readData trainPath
    evalDataM <- case evalPathM of
        Just evalPath
            -> Just . V.fromList 
            . map (CRF.encodeLs [] codec)
            <$> readData evalPath
        Nothing -> return Nothing
    let crf  = CRF.mkModel (CRF.presentFeats trainData)
    para <- sgdM sgdArgs
        (notify sgdArgs crf trainData evalDataM)
        (CRF.mkGrad crf) trainData (CRF.values crf)
    return (codec, crf { CRF.values = para })
    
schematize :: Tr.NeForest NE Word -> [CRF.Labeled Ob Lb]
schematize forest =
    [ CRF.Labeled x [(y, 1.0)]
    | (x, y) <- zip xs ys ]
  where
    iob = IOB.encodeForest forest
    ox = schema (V.fromList $ map IOB.word iob)
    xs = map (execOx . ox) [0 .. length iob - 1]
    ys = map IOB.label iob

readData :: FilePath -> IO [[CRF.Labeled Ob Lb]]
readData path
    = map schematize
    . map (mapForest toString)
    . parseEnamex
   <$> L.readFile path
  where
    toString = Tr.onBoth (L.unpack . L.fromStrict)

type Dataset = V.Vector (CRF.Xs, CRF.Ys)

notify
    :: SgdArgs -> CRF.Model -> Dataset -> Maybe Dataset
    -> Para -> Int -> IO ()
notify SgdArgs{..} crf trainData evalDataM para k 
    | doneTotal k == doneTotal (k - 1) = putStr "."
    | Just dataSet <- evalDataM = do
        let x = CRF.accuracy (crf { CRF.values = para }) (V.toList dataSet)
        putStrLn ("\n" ++ "[" ++ show (doneTotal k) ++ "] f = " ++ show x)
    | otherwise =
        putStrLn ("\n" ++ "[" ++ show (doneTotal k) ++ "] f = #")
  where
    doneTotal :: Int -> Int
    doneTotal = floor . done
    done :: Int -> Double
    done i
        = fromIntegral (i * batchSize)
        / fromIntegral (V.length trainData)
