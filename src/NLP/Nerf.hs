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


import           Control.Applicative ((<$>), (<*>))
import           Data.Binary (Binary, put, get)
import           Data.Foldable (foldMap)
import           Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as L

import           Text.Named.Enamex (parseEnamex)
import qualified Data.Named.Tree as N
import qualified Data.Named.IOB as IOB

import           Numeric.SGD (SgdArgs)
import qualified Data.CRF.Chain1 as CRF

import           NLP.Nerf.Types
import           NLP.Nerf.Tokenize (tokenize, sync)
import           NLP.Nerf.Schema (SchemaConf, Schema, fromConf, schematize)


---------------------
-- Nerf data type
---------------------


-- | A Nerf consists of the observation schema configuration and the CRF model.
data Nerf = Nerf
    { schemaConf    :: SchemaConf
    , crf           :: CRF.CRF Ob Lb }

instance Binary Nerf where
    put Nerf{..} = put schemaConf >> put crf
    get = Nerf <$> get <*> get


---------------------
-- Train
---------------------


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


---------------------
-- NER
---------------------


-- | Perform named entity recognition (NER) using the Nerf model.
ner :: Nerf -> String -> N.NeForest NE Word
ner nerf sent =
    let ws = map T.pack . tokenize $ sent
        schema = fromConf (schemaConf nerf)
        xs = CRF.tag (crf nerf) (schematize schema ws)
    in  IOB.decodeForest [IOB.IOB w x | (w, x) <- zip ws xs]


---------------------
-- Enamex
---------------------


-- | Read data from enamex file and retokenize (so that internal
-- tokenization is used).
readDeep :: FilePath -> IO [N.NeForest NE Word]
readDeep path = map reTokenize . parseEnamex <$> L.readFile path


-- | Like `readDeep` but also converts to the CRF representation.
readFlat :: Schema a -> FilePath -> IO [CRF.SentL Ob Lb]
readFlat schema path = map (flatten schema) <$> readDeep path


---------------------
-- CRF
---------------------


-- | Flatten the forest into a CRF representation.
flatten :: Schema a -> N.NeForest NE Word -> CRF.SentL Ob Lb
flatten schema forest =
    [ CRF.annotate x y
    | (x, y) <- zip xs ys ]
  where
    iob = IOB.encodeForest forest
    xs = schematize schema (map IOB.word iob)
    ys = map IOB.label iob


---------------------
-- Re-tokenization
---------------------


-- | Tokenize sentence with the Nerf tokenizer.
reTokenize :: N.NeForest NE Word -> N.NeForest NE Word
reTokenize ft = 
    sync ft ((doTok . leaves) ft)
  where 
    doTok  = map T.pack . tokenize . intercalate " "  . map T.unpack
    leaves = concatMap $ foldMap (either (const []) (:[]))


---------------------
-- Try OX
---------------------


-- | Show results of observation extraction on the input ENAMEX file.
tryOx :: SchemaConf -> FilePath -> IO ()
tryOx cfg path = do
    input <- readFlat (fromConf cfg) path
    mapM_ drawSent input


drawSent :: CRF.SentL Ob Lb -> IO ()
drawSent sent = do
    let unDist (x, y) = (x, CRF.unDist y)
    mapM_ (print . unDist) sent
    putStrLn "" 


-- ------------------------------------------------------------
-- -- New version (preliminary implementation)
-- ------------------------------------------------------------
-- 
-- 
-- -- | A word enriched with morphosyntactic information.
-- -- TODO: Maybe try a parametrized version, `Morph t`, instead?
-- data Morph = Morph {
--     -- | An orthographic form,
--       orth  :: Word
--     -- | No preceding space.
--     , nps   :: Bool
--     -- | Morphosyntactic description.
--     -- TODO: Use tagset-positional?
--     , msd   :: [T.Text] }
-- 
-- 
-- -- | Perform NER on a morphosyntactically disambiguated sentence.
-- -- No re-tokenizetion is performed.
-- ner' :: (w -> Morph) -> Nerf -> [w]  -> N.NeForest NE w
-- ner' = undefined
-- 
-- 
-- -- | Train Nerf on a morphosyntactically annotated (and disambiguated) data.
-- train'
--     :: SgdArgs              -- ^ Args for SGD
--     -> SchemaConf           -- ^ Observation schema configuration
--     -> FilePath             -- ^ Train data (XCES)
--     -> Maybe FilePath       -- ^ Maybe eval data (XCES)
--     -> IO Nerf              -- ^ The resulting Nerf model
-- train sgdArgs cfg trainPath evalPathM = do
--     let schema = fromConf cfg
--         readTrain = readFlatXCES schema trainPath
--         readEvalM = evalPathM >>= \evalPath ->
--             Just ([], readFlatXCES schema evalPath)
--     _crf <- CRF.train sgdArgs readTrain readEvalM CRF.presentFeats
--     return $ Nerf cfg _crf
