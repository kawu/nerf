{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM_, when)
import System.Console.CmdArgs
import Data.Binary (encodeFile, decodeFile)
import Data.Text.Binary ()
import Text.Named.Enamex (showForest)
import qualified Numeric.SGD as SGD
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import NLP.Nerf (train, ner, tryOx)
import NLP.Nerf.Schema (defaultConf)
import NLP.Nerf.Dict (preparePoliMorf, preparePNEG, prepareNELexicon)

data Args
  = TrainMode
    { trainPath	    :: FilePath
    , neDictPath    :: FilePath
    , evalPath      :: Maybe FilePath
    , iterNum       :: Double
    , batchSize     :: Int
    , regVar        :: Double
    , gain0         :: Double
    , tau           :: Double
    , outNerf       :: FilePath }
  | NerMode
    { dataPath      :: FilePath
    , inNerf        :: FilePath }
  | OxMode
    { dataPath      :: FilePath
    , neDictPath    :: FilePath }
  | PoliMode
    { poliPath      :: FilePath
    , outPath       :: FilePath }
  | PnegMode
    { lmfPath       :: FilePath
    , outPath       :: FilePath }
  | NeLexMode
    { nePath        :: FilePath
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
    , gain0 = 1.0 &= help "Initial gain parameter"
    , tau = 5.0 &= help "Initial tau parameter"
    , outNerf = def &= typFile &= help "Output Nerf file" }

nerMode :: Args
nerMode = NerMode
    { inNerf = def &= argPos 0 &= typ "NERF-FILE"
    , dataPath = def &= argPos 1 &= typ "INPUT" }

oxMode :: Args
oxMode = OxMode
    { dataPath = def &= argPos 0 &= typ "DATA-FILE"
    , neDictPath = def &= argPos 1 &= typ "NE-DICT-FILE" }

poliMode :: Args
poliMode = PoliMode
    { poliPath = def &= typ "PoliMorf" &= argPos 0
    , outPath = def &= typ "Output" &= argPos 1 }

pnegMode :: Args
pnegMode = PnegMode
    { lmfPath = def &= typ "PNEG" &= argPos 0
    , outPath = def &= typ "Output" &= argPos 1 }

neLexMode :: Args
neLexMode = NeLexMode
    { nePath = def &= typ "NELexicon" &= argPos 0
    , outPath = def &= typ "Output" &= argPos 1 }

argModes :: Mode (CmdArgs Args)
argModes = cmdArgsMode $
    modes [trainMode, nerMode, oxMode, poliMode, pnegMode, neLexMode]

main :: IO ()
main = exec =<< cmdArgsRun argModes

exec :: Args -> IO ()

exec TrainMode{..} = do
    cfg  <- defaultConf neDictPath
    nerf <- train sgdArgs cfg trainPath evalPath
    when (not . null $ outNerf) $ do
        putStrLn $ "\nSaving model in " ++ outNerf ++ "..."
        encodeFile outNerf nerf
  where
    sgdArgs = SGD.SgdArgs
        { SGD.batchSize = batchSize
        , SGD.regVar = regVar
        , SGD.iterNum = iterNum
        , SGD.gain0 = gain0
        , SGD.tau = tau }

exec NerMode{..} = do
    nerf  <- decodeFile inNerf
    input <- readRaw dataPath
    forM_ input $ \sent -> do
        let forest = ner nerf sent
        L.putStrLn (showForest forest)

exec OxMode{..} = do
    cfg  <- defaultConf neDictPath
    tryOx cfg dataPath

exec PoliMode{..} = preparePoliMorf poliPath outPath
exec PnegMode{..} = preparePNEG lmfPath outPath
exec NeLexMode{..} = prepareNELexicon nePath outPath

parseRaw :: L.Text -> [[T.Text]]
parseRaw =
    let toStrict = map L.toStrict
    in  map (toStrict . L.words) . L.lines

readRaw :: FilePath -> IO [[T.Text]]
readRaw = fmap parseRaw . L.readFile
