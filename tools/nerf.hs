{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import System.Console.CmdArgs
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, when)
import Data.Maybe (catMaybes)
import Data.Binary (encodeFile, decodeFile)
import Data.Text.Binary ()
import Text.Named.Enamex (showForest)
import qualified Numeric.SGD as SGD
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import NLP.Nerf (train, ner, tryOx)
import NLP.Nerf.Schema (defaultConf)
import NLP.Nerf.Dict
    ( extractPoliMorf, extractPNEG, extractNELexicon, extractProlexbase
    , extractIntTriggers, extractExtTriggers, Dict )

data Args
  = TrainMode
    { trainPath     :: FilePath
    , evalPath      :: Maybe FilePath
    , poliMorfPath  :: Maybe FilePath
    , prolexPath    :: Maybe FilePath
    , pnegPath      :: Maybe FilePath
    , neLexPath     :: Maybe FilePath
    , pnetPath      :: Maybe FilePath
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
    , poliMorfPath  :: Maybe FilePath
    , prolexPath    :: Maybe FilePath
    , pnegPath      :: Maybe FilePath
    , neLexPath     :: Maybe FilePath
    , pnetPath      :: Maybe FilePath }
  deriving (Data, Typeable, Show)

trainMode :: Args
trainMode = TrainMode
    { trainPath = def &= argPos 0 &= typ "TRAIN-FILE"
    , evalPath = def &= typFile &= help "Evaluation file"
    , poliMorfPath = def &= typFile &= help "Path to PoliMorf"
    , prolexPath = def &= typFile &= help "Path to Prolexbase"
    , pnegPath = def &= typFile &= help "Path to PNEG-LMF"
    , neLexPath = def &= typFile &= help "Path to NELexicon"
    , pnetPath = def &= typFile &= help "Path to PNET"
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
    , poliMorfPath = def &= typFile &= help "Path to PoliMorf"
    , prolexPath = def &= typFile &= help "Path to Prolexbase"
    , pnegPath = def &= typFile &= help "Path to PNEG-LMF"
    , neLexPath = def &= typFile &= help "Path to NELexicon"
    , pnetPath = def &= typFile &= help "Path to PNET" }

argModes :: Mode (CmdArgs Args)
argModes = cmdArgsMode $ modes [trainMode, nerMode, oxMode]

data Resources = Resources
    { poliDict      :: Maybe Dict
    , prolexDict    :: Maybe Dict
    , pnegDict      :: Maybe Dict
    , neLexDict     :: Maybe Dict
    , intDict       :: Maybe Dict
    , extDict       :: Maybe Dict }

extract :: Args -> IO Resources
extract as = Resources
    <$> inject extractPoliMorf (poliMorfPath as)
    <*> inject extractProlexbase (prolexPath as)
    <*> inject extractPNEG (pnegPath as)
    <*> inject extractNELexicon (neLexPath as)
    <*> inject extractIntTriggers (pnetPath as)
    <*> inject extractExtTriggers (pnetPath as)

inject :: (a -> IO b) -> Maybe a -> IO (Maybe b)
inject f (Just x) = do
    y <- f x
    return (Just y)
inject _ Nothing = return Nothing

main :: IO ()
main = exec =<< cmdArgsRun argModes

exec :: Args -> IO ()

exec as@TrainMode{..} = do
    Resources{..} <- extract as
    cfg <- defaultConf
        (catMaybes [poliDict, prolexDict, pnegDict, neLexDict])
        intDict extDict
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

exec as@OxMode{..} = do
    Resources{..} <- extract as
    cfg <- defaultConf
        (catMaybes [poliDict, prolexDict, pnegDict, neLexDict])
        intDict extDict
    tryOx cfg dataPath

parseRaw :: L.Text -> [[T.Text]]
parseRaw =
    let toStrict = map L.toStrict
    in  map (toStrict . L.words) . L.lines

readRaw :: FilePath -> IO [[T.Text]]
readRaw = fmap parseRaw . L.readFile
