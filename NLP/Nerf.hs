{-# LANGUAGE RecordWildCards #-}

module NLP.Nerf
( train
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)
import Control.Monad.Lazy (forM)
import Control.Arrow (second)
import Data.Char (toLower)
import Data.Maybe (mapMaybe)
import System.IO (hSetBuffering, stdout, BufferMode (..))
import qualified Data.Set as S
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

import qualified Data.CRF as CRF

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

schematize :: Tr.NeForest NE Word -> CRF.SentL Ob Lb
schematize forest =
    [ CRF.annotate y (S.fromList x)
    | (x, y) <- zip xs ys ]
  where
    iob = IOB.encodeForest forest
    ox = schema (V.fromList $ map IOB.word iob)
    xs = map (execOx . ox) [0 .. length iob - 1]
    ys = map IOB.label iob

readData :: FilePath -> IO [CRF.SentL Ob Lb]
readData path
    = map schematize
    . map (mapForest toString)
    . parseEnamex
   <$> L.readFile path
  where
    toString = Tr.onBoth (L.unpack . L.fromStrict)

-- | Train the CRF model
train
    :: SgdArgs                      -- ^ Args for SGD
    -> FilePath                     -- ^ Train data (ENAMEX)
    -> Maybe FilePath               -- ^ Maybe eval data (ENAMEX)
    -> IO (CRF.Codec Ob Lb, CRF.Model)    -- ^ Resulting codec and model
train sgdArgs trainPath evalPathM = do
    let readTrain = readData trainPath
        readEvalM = evalPathM >>= \evalPath ->
            Just ([], readData evalPath)
    CRF.train sgdArgs readTrain readEvalM CRF.presentFeats
