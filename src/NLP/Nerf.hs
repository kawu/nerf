{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Main module of the Nerf tool.


module NLP.Nerf
( Nerf (..)
, train
, train'
, ner
, tryOx
, module NLP.Nerf.Types
) where


import           Control.Applicative ((<$>), (<*>))
import           Data.Binary (Binary, put, get)
import           Data.Foldable (foldMap)
import           Data.List (intercalate)
import qualified Data.Map as M
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
import qualified NLP.Nerf.XCES2 as XCES


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
ner :: Nerf -> String -> N.NeForest NE T.Text
ner nerf sent =
    -- TODO: we could try to recover `nps` attributes.
    let mkWord x = Word {orth = x, nps = False, msd = []}
        ws = map T.pack . tokenize $ sent
        schema = fromConf (schemaConf nerf)
        xs = CRF.tag (crf nerf) (schematize schema $ map mkWord ws)
    in  IOB.decodeForest [IOB.IOB w x | (w, x) <- zip ws xs]


---------------------
-- Enamex
---------------------


-- | Read data from enamex file and retokenize (so that internal
-- tokenization is used).
readDeep :: FilePath -> IO [N.NeForest NE Word]
readDeep path
    = map (mkForest . reTokenize)
    . parseEnamex <$> L.readFile path
  where
    mkForest = N.mapForest $ N.onEither mkNE mkWord
    mkNE x = M.singleton "" x
    -- TODO: we could try to recover `nps` attributes.
    mkWord x = Word {orth = x, nps = False, msd = []}


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
reTokenize :: N.NeForest a T.Text -> N.NeForest a T.Text
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


------------------------------------------------------------
-- New version (preliminary implementation)
------------------------------------------------------------


-- | Perform NER on a morphosyntactically disambiguated sentence.
-- No re-tokenizetion is performed.
ner' :: (w -> Word) -> Nerf -> [w]  -> N.NeForest NE w
ner' f nerf ws =
    let schema = fromConf (schemaConf nerf)
        xs = CRF.tag (crf nerf) (schematize schema $ map f ws)
    in  IOB.decodeForest [IOB.IOB w x | (w, x) <- zip ws xs]


-- | Train Nerf on a morphosyntactically annotated (and disambiguated) data.
train'
    :: SgdArgs              -- ^ Args for SGD
    -> SchemaConf           -- ^ Observation schema configuration
    -> FilePath             -- ^ Train data (XCES)
    -> Maybe FilePath       -- ^ Maybe eval data (XCES)
    -> IO Nerf              -- ^ The resulting Nerf model
train' sgdArgs cfg trainPath evalPathM = do
    let schema = fromConf cfg
        readTrain = readFlatXCES schema trainPath
        readEvalM = evalPathM >>= \evalPath ->
            Just ([], readFlatXCES schema evalPath)
    -- mapM (mapM showIt) =<< readDeepXCES trainPath
    -- mapM (mapM print) . XCES.parseXCES =<< L.readFile trainPath
    _crf <- CRF.train sgdArgs readTrain readEvalM CRF.presentFeats
    return $ Nerf cfg _crf


-- | Read data from the XCES.
--
-- TODO: specify `ns`s.
readDeepXCES :: FilePath -> IO [[N.NeForest NE Word]]
readDeepXCES = 
    let prep = (map.map) XCES.fromXCES . XCES.parseXCES
    in  fmap prep . L.readFile


-- | Like `readDeep` but also converts to the CRF representation.
readFlatXCES :: Schema a -> FilePath -> IO [CRF.SentL Ob Lb]
readFlatXCES schema path = map (flatten schema) . concat <$> readDeepXCES path
