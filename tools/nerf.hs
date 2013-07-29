{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import           System.Console.CmdArgs
import           System.IO
    ( Handle, hGetBuffering, hSetBuffering
    , stdout, BufferMode (..), hClose )
import qualified System.IO.Temp as Temp
import           System.Directory (getDirectoryContents)
import           System.FilePath (takeBaseName, (</>), (<.>))
import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow (second)
import           Control.Monad (forM_)
import           Data.Maybe (catMaybes)
import           Data.Binary (encodeFile, decodeFile)
import           Data.Text.Binary ()
import           Text.Named.Enamex (parseEnamex, showForest)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Numeric.SGD as SGD
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.DAWG.Static as D

import           NLP.Nerf (train, ner, tryOx)
import           NLP.Nerf.Schema (defaultConf)
import           NLP.Nerf.Dict
    ( extractPoliMorf, extractPNEG, extractNELexicon, extractProlexbase
    , extractIntTriggers, extractExtTriggers, Dict )

import           NLP.Nerf.Compare ((.+.))
import qualified NLP.Nerf.Compare as C

data Nerf
  = Train
    { trainPath     :: FilePath
    , evalPath      :: Maybe FilePath
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
    , outNerf       :: Maybe FilePath }
  | CV
    { dataDir       :: FilePath
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
    , outDir        :: Maybe FilePath }
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
  | Compare
    { dataPath      :: FilePath
    , dataPath'     :: FilePath }
  deriving (Data, Typeable, Show)

trainMode :: Nerf
trainMode = Train
    { trainPath = def &= argPos 0 &= typ "TRAIN-FILE"
    , evalPath = def &= typFile &= help "Evaluation file"
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
    , outNerf = def &= typFile &= help "Output model file" }

cvMode :: Nerf
cvMode = CV
    { dataDir = def &= argPos 0 &= typ "DATA-DIR"
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
    , outDir = def &= typFile &= help "Output model directory" }

nerMode :: Nerf
nerMode = NER
    { inNerf   = def &= argPos 0 &= typ "NERF-FILE"
    , dataPath = def &= argPos 1 &= typ "INPUT" }

oxMode :: Nerf
oxMode = Ox
    { dataPath = def &= argPos 0 &= typ "DATA-FILE"
    , poliMorf = def &= typFile &= help "Path to PoliMorf"
    , prolex = def &= typFile &= help "Path to Prolexbase"
    , pneg = def &= typFile &= help "Path to PNEG-LMF"
    , neLex = def &= typFile &= help "Path to NELexicon"
    , pnet = def &= typFile &= help "Path to PNET" }

cmpMode :: Nerf
cmpMode = Compare
    { dataPath  = def &= argPos 0 &= typ "REFERENCE"
    , dataPath' = def &= argPos 1 &= typ "COMPARED" }

argModes :: Mode (CmdArgs Nerf)
argModes = cmdArgsMode $ modes [trainMode, cvMode, nerMode, cmpMode, oxMode]

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
    putStrLn $ "Number of automaton states = " ++ show k
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
    nerf <- train sgdArgs cfg trainPath evalPath
    flip F.traverse_ outNerf $ \path -> do
        putStrLn $ "\nSaving model in " ++ path ++ "..."
        encodeFile path nerf
  where
    sgdArgs = SGD.SgdArgs
        { SGD.batchSize = batchSize
        , SGD.regVar = regVar
        , SGD.iterNum = iterNum
        , SGD.gain0 = gain0
        , SGD.tau = tau }

exec nerfArgs@CV{..} = do
    Resources{..} <- extract nerfArgs
    cfg <- defaultConf
        (catMaybes [poliDict, prolexDict, pnegDict, neLexDict])
        intDict extDict
    parts <- getParts dataDir
    forM_ (enumDivs parts) $ \(evalPath, trainPaths) -> do
        putStrLn $ "\nPart: " ++ evalPath
        withParts trainPaths $ \trainPath -> do
            nerf <- train sgdArgs cfg trainPath (Just evalPath)
            flip F.traverse_ outDir $ \dir -> do
                let path = dir </> takeBaseName evalPath <.> ".bin"
                putStrLn $ "\nSaving model in " ++ path ++ "..."
                encodeFile path nerf
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
        let forest = ner nerf (L.unpack sent)
        L.putStrLn (showForest forest)

exec nerfArgs@Ox{..} = do
    Resources{..} <- extract nerfArgs
    cfg <- defaultConf
        (catMaybes [poliDict, prolexDict, pnegDict, neLexDict])
        intDict extDict
    tryOx cfg dataPath

exec Compare{..} = do
    x <- parseEnamex <$> L.readFile dataPath
    y <- parseEnamex <$> L.readFile dataPath'
    let statMap = C.compare $ zip x y
    forM_ (M.toList statMap) $ uncurry printStats
    printStats "<all>" (foldl1 (.+.) $ M.elems statMap)
  where
    printStats neType stats = do
        putStrLn $ "# " ++ T.unpack neType
        putStrLn $ "true positive: "    ++ show (C.tp stats)
        putStrLn $ "false positive: "   ++ show (C.fp stats)
        -- putStrLn $ "true negative: "    ++ show (C.tn stats)
        putStrLn $ "false negative: "   ++ show (C.fn stats)

readRaw :: FilePath -> IO [L.Text]
readRaw = fmap L.lines . L.readFile


----------------------------------------
-- Cross-validation
----------------------------------------


-- | Get paths of the individual parts of the dataset
-- stored in the given directory.
getParts :: FilePath -> IO [FilePath]
getParts path = do
    xs <- filter (\x -> not (x `elem` [".", ".."]))
      <$> getDirectoryContents path
    return $ map (path </>) xs


-- | Take data from the given list of paths and store
-- it all in a temporary file, than run the given handler.
withParts :: [FilePath] -> (FilePath -> IO a) -> IO a
withParts paths handler = Temp.withSystemTempFile "train." $ \tempPath _h -> do
    hClose _h
    forM_ paths $ \srcPath -> do
        L.readFile srcPath >>= L.appendFile tempPath
    handler tempPath


-- | Enumerate subsequent partitionings of the dataset.
enumDivs :: [a] -> [(a, [a])]
enumDivs []     = []
enumDivs (x:xs) = (x, xs) : map (second (x:)) (enumDivs xs)
