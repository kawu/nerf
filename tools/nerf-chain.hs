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
import NLP.Nerf (train, ner)
import NLP.Nerf.Dict
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

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
  deriving (Data, Typeable, Show)

trainMode :: Args
trainMode = TrainMode
    { trainPath = def &= argPos 0 &= typ "TRAIN-FILE"
    , neDictPath = def &= argPos 1 &= typ "NE-DICT-FILE"
    , evalPath = def &= typFile &= help "Evaluation data file"
    , iterNum = 10 &= help "Number of SGD iterations"
    , batchSize = 30 &= help "Batch size"
    , regVar = 10.0 &= help "Regularization variance"
    , scale0 = 1.0 &= help "Initial scale parameter"
    , tau = 5.0 &= help "Initial tau parameter"
    , outModel = def &= typFile &= help "Output model file" }

nerMode :: Args
nerMode = NerMode
    { loadModel = def &= argPos 0 &= typ "MODEL"
    , neDictPath = def &= argPos 1 &= typ "NE-DICT-FILE"
    , dataPath = def &= typFile
        &= help "Input file; if not specified, read from stdin" }

argModes :: Mode (CmdArgs Args)
argModes = cmdArgsMode $ modes [trainMode, nerMode]

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

parseRaw :: L.Text -> [[T.Text]]
parseRaw =
    let toStrict = map L.toStrict
    in  map (toStrict . L.words) . L.lines

readRaw :: FilePath -> IO [[T.Text]]
readRaw = fmap parseRaw . L.readFile
