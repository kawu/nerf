{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main module of the Nerf tool.

module NLP.Nerf
( train
, tag
) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Char as C
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as L

import qualified Control.Monad.Ox as Ox
import qualified Control.Monad.Ox.Text as Ox

import Text.Named.Enamex (parseEnamex)
import qualified Data.Named.Tree as Tr
import qualified Data.Named.IOB as IOB

import Numeric.SGD (SgdArgs)
import qualified Data.CRF.Chain1 as CRF

type Word = T.Text
type NE = T.Text
type Ob = ([Int], T.Text)
type Lb = IOB.Label T.Text

-- | TODO: Add dictionary lookup.
schema :: V.Vector Word -> Int -> Ox.Ox Word T.Text ()
schema sent = \k -> do
    mapM_ (Ox.save . lowOrth)       [k - 1, k]
    mapM_ (Ox.save . upOnlyOrth)    [k - 1, k]
    mapM_ lowLemma                  [k - 1, k]
    mapM_ (Ox.save . shape)         [k - 1, k]
    mapM_ (Ox.save . shapeP)        [k - 1, k]
    Ox.save $ link <$> shape  k <*> shape  (k - 1)
    Ox.save $ link <$> shapeP k <*> shapeP (k - 1)
  where
    at              = Ox.atWith sent
    orth            = (id `at`)
    lowOrth i       = T.toLower <$> orth i
    upOnlyOrth i    = orth i >>= \x -> case T.any C.isUpper x of
        True    -> Just x
        False   -> Nothing

    shape i         = Ox.shape <$> orth i
    shapeP i        = Ox.pack <$> shape i
    link x y        = T.concat [x, "-", y]

    lowLemma i = Ox.group $ do
        mapM_ (Ox.save . lowPrefix i) [0, -1, -2, -3]
        mapM_ (Ox.save . lowSuffix i) [0, -1, -2, -3]

    lowPrefix i j = Ox.prefix j =<< lowOrth i
    lowSuffix i j = Ox.suffix j =<< lowOrth i

schematize :: [Word] -> CRF.Sent Ob
schematize xs =
    map (S.fromList . Ox.execOx . schema v) [0 .. n - 1]
  where
    v = V.fromList xs
    n = V.length v

flatten :: Tr.NeForest NE Word -> CRF.SentL Ob Lb
flatten forest =
    [ CRF.annotate x y
    | (x, y) <- zip xs ys ]
  where
    iob = IOB.encodeForest forest
    xs = schematize (map IOB.word iob)
    ys = map IOB.label iob

readFlat :: FilePath -> IO [CRF.SentL Ob Lb]
readFlat path = map flatten . parseEnamex <$> L.readFile path

-- | Train the CRF model.
train
    :: SgdArgs              -- ^ Args for SGD
    -> FilePath             -- ^ Train data (ENAMEX)
    -> Maybe FilePath       -- ^ Maybe eval data (ENAMEX)
    -> IO (CRF.CRF Ob Lb)   -- ^ Resulting codec and model
train sgdArgs trainPath evalPathM = do
    let readTrain = readFlat trainPath
        readEvalM = evalPathM >>= \evalPath ->
            Just ([], readFlat evalPath)
    CRF.train sgdArgs readTrain readEvalM CRF.presentFeats

-- | Tag with the CRF model.
tag :: CRF.CRF Ob Lb -> [Word] -> Tr.NeForest NE Word
tag crf ws = 
    let xs = CRF.tag crf (schematize ws)
    in  IOB.decodeForest [IOB.IOB w x | (w, x) <- zip ws xs]
