{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main module of the Nerf tool.

module NLP.Nerf
( Nerf (..)
, train
, ner
, tryOx
, module NLP.Nerf.Types
) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, put, get)
import Data.Foldable (foldMap)
import qualified Data.Text.Lazy.IO as L

import Text.Named.Enamex (parseEnamex)
import qualified Data.Named.Tree as Tr
import qualified Data.Named.IOB as IOB

import Numeric.SGD (SgdArgs)
import qualified Data.CRF.Chain1 as CRF

import NLP.Nerf.Types
import NLP.Nerf.Tokenize (moveNEs)
import NLP.Nerf.Schema (SchemaConf, Schema, fromConf, schematize)

-- | A Nerf consists of the observation schema configuration and the CRF model.
data Nerf = Nerf
    { schemaConf    :: SchemaConf
    , crf           :: CRF.CRF Ob Lb }

instance Binary Nerf where
    put Nerf{..} = put schemaConf >> put crf
    get = Nerf <$> get <*> get

flatten :: Schema a -> Tr.NeForest NE Word -> CRF.SentL Ob Lb
flatten schema forest =
    [ CRF.annotate x y
    | (x, y) <- zip xs ys ]
  where
    iob = IOB.encodeForest forest
    xs = schematize schema (map IOB.word iob)
    ys = map IOB.label iob

-- | Tokenize sentence with the Nerf tokenizer.
reTokenize :: Tr.NeForest NE Word -> Tr.NeForest NE Word
reTokenize ft = 
    let leaves = concatMap $ foldMap (either (const []) (:[]))
    in  moveNEs ft (leaves ft)

readDeep :: FilePath -> IO [Tr.NeForest NE Word]
readDeep path = map reTokenize . parseEnamex <$> L.readFile path

readFlat :: Schema a -> FilePath -> IO [CRF.SentL Ob Lb]
readFlat schema path = map (flatten schema) <$> readDeep path

drawSent :: CRF.SentL Ob Lb -> IO ()
drawSent sent = do
    let unDist (x, y) = (x, CRF.unDist y)
    mapM_ (print . unDist) sent
    putStrLn "" 

-- | Show results of observation extraction on the input ENAMEX file.
tryOx :: SchemaConf -> FilePath -> IO ()
tryOx cfg path = do
    input <- readFlat (fromConf cfg) path
    mapM_ drawSent input

-- | Train Nerf on the input data using the SGD method.
train
    :: SgdArgs              -- ^ Args for SGD
    -> SchemaConf           -- ^ Observation schema configuration
    -> FilePath             -- ^ Train data (ENAMEX)
    -> Maybe FilePath       -- ^ Maybe eval data (ENAMEX)
    -> IO Nerf              -- ^ Nerf with resulting codec and model
train sgdArgs cfg trainPath evalPathM = do
    let schema = fromConf cfg
        readTrain = readFlat schema trainPath
        readEvalM = evalPathM >>= \evalPath ->
            Just ([], readFlat schema evalPath)
    _crf <- CRF.train sgdArgs readTrain readEvalM CRF.presentFeats
    return $ Nerf cfg _crf

-- | Perform named entity recognition (NER) using the Nerf.
ner :: Nerf -> [Word] -> Tr.NeForest NE Word
ner nerf ws =
    let schema = fromConf (schemaConf nerf)
        xs = CRF.tag (crf nerf) (schematize schema ws)
    in  IOB.decodeForest [IOB.IOB w x | (w, x) <- zip ws xs]
