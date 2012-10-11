{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM_)
import System.Console.CmdArgs
import Data.Binary (encodeFile, decodeFile)
import Data.Text.Binary ()
import Numeric.SGD (sgdArgsDefault)
import Text.Named.Enamex (showForest)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import NLP.Nerf (train, ner)
import NLP.Nerf.Dict (prepareLMF, prepareNeLexicon)

data Args
  = TrainMode
    { trainPath	    :: FilePath
    , neDictPath    :: FilePath
    , evalPath      :: Maybe FilePath
    , iterNum       :: Double
    , batchSize     :: Int
    , regVar        :: Double
    , scale0        :: Double
    , tau           :: Double
    , outModel      :: FilePath }
  | NerMode
    { dataPath      :: FilePath
    , neDictPath    :: FilePath
    , loadModel     :: FilePath }
  | LmfMode
    { lmfPath       :: FilePath
    , outPath       :: FilePath }
  | NeLexMode
    { nePath        :: FilePath
    , poliPath      :: FilePath
    , outPath       :: FilePath }
  deriving (Data, Typeable, Show)

trainMode :: Args
trainMode = TrainMode
    { trainPath = def &= argPos 0 &= typ "TRAIN-FILE"
    , neDictPath = def &= argPos 1 &= typ "NE-DICT-FILE"
    , evalPath = def &= typFile &= help "Evaluation file"
    , iterNum = 10 &= help "Number of SGD iterations"
    , batchSize = 30 &= help "Batch size"
    , regVar = 10.0 &= help "Regularization variance"
    , scale0 = 1.0 &= help "Initial scale parameter"
    , tau = 5.0 &= help "Initial tau parameter"
    , outModel = def &= typFile &= help "Output CRF file" }

nerMode :: Args
nerMode = NerMode
    { loadModel = def &= argPos 0 &= typ "CRF"
    , neDictPath = def &= argPos 1 &= typ "NE-DICT-FILE"
    , dataPath = def &= typFile &= help "Input" }
        -- &= help "Input file; if not specified, read from stdin" }

lmfMode :: Args
lmfMode = LmfMode
    { lmfPath = def &= typ "LMF" &= argPos 0
    , outPath = def &= typ "Output" &= argPos 1 }

neLexMode :: Args
neLexMode = NeLexMode
    { nePath = def &= typ "NELexicon" &= argPos 0
    , poliPath = def &= typ "PoliMorf" &= argPos 1
    , outPath = def &= typ "Output" &= argPos 2 }

argModes :: Mode (CmdArgs Args)
argModes = cmdArgsMode $ modes [trainMode, nerMode, lmfMode, neLexMode]

main :: IO ()
main = do
    args <- cmdArgsRun argModes
    exec args

exec :: Args -> IO ()

-- | FIXME: Do not ignore SGD arguments.
exec TrainMode{..} = do
    neDict <- decodeFile neDictPath
    crf <- train sgdArgsDefault neDict trainPath evalPath
    encodeFile outModel crf

exec NerMode{..} = do
    neDict <- decodeFile neDictPath
    crf <- decodeFile loadModel
    input <- readRaw dataPath
    forM_ input $ \sent -> do
        let forest = ner neDict crf sent
        L.putStrLn (showForest forest)

exec LmfMode{..} = prepareLMF lmfPath outPath
exec NeLexMode{..} = prepareNeLexicon nePath poliPath outPath

parseRaw :: L.Text -> [[T.Text]]
parseRaw =
    let toStrict = map L.toStrict
    in  map (toStrict . L.words) . L.lines

readRaw :: FilePath -> IO [[T.Text]]
readRaw = fmap parseRaw . L.readFile
