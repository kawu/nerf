{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import System.Console.CmdArgs
import System.IO
    ( Handle, hGetBuffering, hSetBuffering
    , stdout, BufferMode (..) )
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
import qualified Data.DAWG.Static as D

import NLP.Nerf (train, ner, tryOx)
import NLP.Nerf.Schema (defaultConf)
import NLP.Nerf.Tokenize (tokenize)
import NLP.Nerf.Dict
    ( extractPoliMorf, extractPNEG, extractNELexicon, extractProlexbase
    , extractIntTriggers, extractExtTriggers, Dict )

data Nerf
  = Train
    { trainPath     :: FilePath
    , eval          :: Maybe FilePath
    , poliMorf      :: Maybe FilePath
    , prolex        :: Maybe FilePath
    , pneg          :: Maybe FilePath
    , neLex         :: Maybe FilePath
    , pnet          :: Maybe FilePath
    , iterNum       :: Double
    , batchSize     :: Int
    , regVar        :: Double
    , gain0         :: Double
    , tau           :: Double
    , outNerf       :: FilePath }
  | NER
    { dataPath      :: FilePath
    , inNerf        :: FilePath }
  | Ox
    { dataPath      :: FilePath
    , poliMorf      :: Maybe FilePath
    , prolex        :: Maybe FilePath
    , pneg          :: Maybe FilePath
    , neLex         :: Maybe FilePath
    , pnet          :: Maybe FilePath }
  deriving (Data, Typeable, Show)

trainMode :: Nerf
trainMode = Train
    { trainPath = def &= argPos 0 &= typ "TRAIN-FILE"
    , eval = def &= typFile &= help "Evaluation file"
    , poliMorf = def &= typFile &= help "Path to PoliMorf"
    , prolex = def &= typFile &= help "Path to Prolexbase"
    , pneg = def &= typFile &= help "Path to PNEG-LMF"
    , neLex = def &= typFile &= help "Path to NELexicon"
    , pnet = def &= typFile &= help "Path to PNET"
    , iterNum = 10 &= help "Number of SGD iterations"
    , batchSize = 30 &= help "Batch size"
    , regVar = 10.0 &= help "Regularization variance"
    , gain0 = 1.0 &= help "Initial gain parameter"
    , tau = 5.0 &= help "Initial tau parameter"
    , outNerf = def &= typFile &= help "Output Nerf file" }

nerMode :: Nerf
nerMode = NER
    { inNerf = def &= argPos 0 &= typ "NERF-FILE"
    , dataPath = def &= argPos 1 &= typ "INPUT" }

oxMode :: Nerf
oxMode = Ox
    { dataPath = def &= argPos 0 &= typ "DATA-FILE"
    , poliMorf = def &= typFile &= help "Path to PoliMorf"
    , prolex = def &= typFile &= help "Path to Prolexbase"
    , pneg = def &= typFile &= help "Path to PNEG-LMF"
    , neLex = def &= typFile &= help "Path to NELexicon"
    , pnet = def &= typFile &= help "Path to PNET" }

argModes :: Mode (CmdArgs Nerf)
argModes = cmdArgsMode $ modes [trainMode, nerMode, oxMode]

data Resources = Resources
    { poliDict      :: Maybe Dict
    , prolexDict    :: Maybe Dict
    , pnegDict      :: Maybe Dict
    , neLexDict     :: Maybe Dict
    , intDict       :: Maybe Dict
    , extDict       :: Maybe Dict }

extract :: Nerf -> IO Resources
extract nerf = withBuffering stdout NoBuffering $ Resources
    <$> extractDict "PoliMorf" extractPoliMorf (poliMorf nerf)
    <*> extractDict "Prolexbase" extractProlexbase (prolex nerf)
    <*> extractDict "PNEG" extractPNEG (pneg nerf)
    <*> extractDict "NELexicon" extractNELexicon (neLex nerf)
    <*> extractDict "internal triggers" extractIntTriggers (pnet nerf)
    <*> extractDict "external triggers" extractExtTriggers (pnet nerf)

withBuffering :: Handle -> BufferMode -> IO a -> IO a
withBuffering h mode io = do
    oldMode <- hGetBuffering h
    hSetBuffering h mode
    x <- io
    hSetBuffering h oldMode
    return x

extractDict :: String -> (a -> IO Dict) -> Maybe a -> IO (Maybe Dict)
extractDict msg f (Just x) = do
    putStr $ "Reading " ++ msg ++ "..."
    dict <- f x
    let k = D.numStates dict
    k `seq` putStrLn $ " Done"
    putStrLn $ "Number of automata states = " ++ show k
    return (Just dict)
extractDict _ _ Nothing = return Nothing

main :: IO ()
main = exec =<< cmdArgsRun argModes

exec :: Nerf -> IO ()

exec nerfArgs@Train{..} = do
    Resources{..} <- extract nerfArgs
    cfg <- defaultConf
        (catMaybes [poliDict, prolexDict, pnegDict, neLexDict])
        intDict extDict
    nerf <- train sgdArgs cfg trainPath eval
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

exec NER{..} = do
    nerf  <- decodeFile inNerf
    input <- readRaw dataPath
    forM_ input $ \sent -> do
        let forest = ner nerf sent
        L.putStrLn (showForest forest)

exec nerfArgs@Ox{..} = do
    Resources{..} <- extract nerfArgs
    cfg <- defaultConf
        (catMaybes [poliDict, prolexDict, pnegDict, neLexDict])
        intDict extDict
    tryOx cfg dataPath

-- | Prepare input data: divide it into a list of sentences and tokenize
-- each sentence using the default tokenizer.
parseRaw :: L.Text -> [[T.Text]]
parseRaw =
    let doTok = map T.pack . tokenize . L.unpack
    in  map doTok . L.lines

readRaw :: FilePath -> IO [[T.Text]]
readRaw = fmap parseRaw . L.readFile
