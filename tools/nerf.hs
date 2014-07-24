{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import           System.Console.CmdArgs
import           System.IO
    ( Handle, hGetBuffering, hSetBuffering
    , stdout, BufferMode (..), hClose, hFlush )
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.IO.Temp as Temp
import qualified Network as N
import           System.Directory (getDirectoryContents)
import           System.FilePath (takeBaseName, (</>), (<.>))
import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow (second)
import           Control.Monad (forM_)
import           Data.Maybe (catMaybes)
-- import           Data.Binary (encodeFile, decodeFile)
import           Data.Text.Binary ()
import           Text.Named.Enamex (parseEnamex, showForest)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Numeric.SGD as SGD
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.DAWG.Static as D

-- Positional tagset
import qualified Data.Tagset.Positional as P

import qualified NLP.Nerf as Nerf
import           NLP.Nerf.Schema (defaultConf)
import           NLP.Nerf.Dict
    ( extractPoliMorf, extractPNEG, extractNELexicon, extractProlexbase
    , extractIntTriggers, extractExtTriggers, Dict )
-- import           NLP.Nerf.XCES as XCES
import qualified NLP.Nerf.Server as S

import           NLP.Nerf.Compare ((.+.))
import qualified NLP.Nerf.Compare as C

-- For the NKJP2XCES mode:
import qualified NLP.Nerf.XCES2 as XCES2
import qualified Data.Named.Tree as NETree
import qualified Text.NKJP.Named as NKJP.NE
-- import qualified Text.NKJP.Morphosyntax as NKJP.MX

import           Paths_nerf (version, getDataFileName)
import           Data.Version (showVersion)



---------------------------------------
-- Configuration
---------------------------------------


-- | Default port number.
portDefault :: Int
portDefault = 10090


---------------------------------------
-- Command line options
---------------------------------------


-- | Data formats. 
data Format
    = Text
    | XCES
    deriving (Data, Typeable, Show)


-- | A description of the Concraft-pl tool        
nerfDesc :: String             
nerfDesc = "Nerf " ++ showVersion version


data Nerf
  = Train
    { trainPath     :: FilePath
    , evalPath      :: Maybe FilePath
    , format        :: Format
    , tagsetPath    :: Maybe FilePath
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
    , tagsetPath    :: Maybe FilePath
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
    { inModel       :: FilePath
    , format        :: Format }
  | Server
    { inModel       :: FilePath
    , port          :: Int }
  | Client
    { format        :: Format
    , tagsetPath    :: Maybe FilePath
    , host          :: String
    , port          :: Int }
  | Ox
    { dataPath      :: FilePath
    , format        :: Format
    , tagsetPath    :: Maybe FilePath
    , poliMorf      :: Maybe FilePath
    , prolex        :: Maybe FilePath
    , pneg          :: Maybe FilePath
    , neLex         :: Maybe FilePath
    , pnet          :: Maybe FilePath }
  | Compare
    { dataPath      :: FilePath
    , dataPath'     :: FilePath }
  | NKJP2XCES
    { nkjpPath      :: FilePath }
  deriving (Data, Typeable, Show)


trainMode :: Nerf
trainMode = Train
    { trainPath = def &= argPos 0 &= typ "TRAIN-FILE"
    , evalPath = def &= typFile &= help "Evaluation file"
    , format = enum
        [ Text &= help "Enamex"
        , XCES &= help "XCES" ]
    , tagsetPath = def &= typFile &= help "Tagset definition file"
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
    , tagsetPath = def &= typFile &= help "Tagset definition file"
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
    { inModel  = def &= argPos 0 &= typ "MODEL-FILE"
    , format   = enum
        [ Text &= help "Raw text"
        , XCES &= help "XCES" ] }


serverMode :: Nerf
serverMode = Server
    { inModel = def &= argPos 0 &= typ "MODEL-FILE"
    , port    = portDefault &= help "Port number" }


clientMode :: Nerf
clientMode = Client
    { port   = portDefault &= help "Port number"
    , tagsetPath = def &= typFile &= help "Tagset definition file"
    , host   = "localhost" &= help "Server host name"
    , format   = enum
        [ Text &= help "Raw text"
        , XCES &= help "XCES" ] }


oxMode :: Nerf
oxMode = Ox
    { dataPath = def &= argPos 0 &= typ "DATA-FILE"
    , format = enum
        [ Text &= help "Enamex"
        , XCES &= help "XCES" ]
    , tagsetPath = def &= typFile &= help "Tagset definition file"
    , poliMorf = def &= typFile &= help "Path to PoliMorf"
    , prolex = def &= typFile &= help "Path to Prolexbase"
    , pneg = def &= typFile &= help "Path to PNEG-LMF"
    , neLex = def &= typFile &= help "Path to NELexicon"
    , pnet = def &= typFile &= help "Path to PNET" }


cmpMode :: Nerf
cmpMode = Compare
    { dataPath  = def &= argPos 0 &= typ "REFERENCE"
    , dataPath' = def &= argPos 1 &= typ "COMPARED" }


nkjp2xcesMode :: Nerf
nkjp2xcesMode = NKJP2XCES
    { nkjpPath  = def &= argPos 0 &= typ "NKJP.tar.bz2" }


argModes :: Mode (CmdArgs Nerf)
argModes = cmdArgsMode $ modes
    [ trainMode, cvMode, nerMode, serverMode, clientMode
    , cmpMode, oxMode, nkjp2xcesMode ]
    &= summary nerfDesc
    &= program "nerf"


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
    -- Dictionaries
    Resources{..} <- extract nerfArgs
    cfg <- defaultConf
        (catMaybes [poliDict, prolexDict, pnegDict, neLexDict])
        intDict extDict

    -- Tagset
    tagsetPath' <- case tagsetPath of
        Nothing -> getDataFileName "config/nkjp-tagset.cfg"
        Just x  -> return x     
    tagset <- P.parseTagset tagsetPath' <$> readFile tagsetPath'

    -- Format
    let doTrain = case format of
            Text    -> Nerf.train
            XCES    -> Nerf.train'

    -- Training proper
    nerf <- doTrain tagset sgdArgs cfg trainPath evalPath
    flip F.traverse_ outNerf $ \path -> do
        putStrLn $ "\nSaving model in " ++ path ++ "..."
        Nerf.saveModel path nerf

  where
    
    -- SGD parameters
    sgdArgs = SGD.SgdArgs
        { SGD.batchSize = batchSize
        , SGD.regVar = regVar
        , SGD.iterNum = iterNum
        , SGD.gain0 = gain0
        , SGD.tau = tau }


exec nerfArgs@CV{..} = do
    -- Dictionaries
    Resources{..} <- extract nerfArgs
    cfg <- defaultConf
        (catMaybes [poliDict, prolexDict, pnegDict, neLexDict])
        intDict extDict

    -- Tagset
    tagsetPath' <- case tagsetPath of
        Nothing -> getDataFileName "config/nkjp-tagset.cfg"
        Just x  -> return x     
    tagset <- P.parseTagset tagsetPath' <$> readFile tagsetPath'

    -- CV proper
    parts <- getParts dataDir
    forM_ (enumDivs parts) $ \(evalPath, trainPaths) -> do
        putStrLn $ "\nPart: " ++ evalPath
        withParts trainPaths $ \trainPath -> do
            nerf <- Nerf.train tagset sgdArgs cfg trainPath (Just evalPath)
            flip F.traverse_ outDir $ \dir -> do
                let path = dir </> takeBaseName evalPath <.> ".bin"
                putStrLn $ "\nSaving model in " ++ path ++ "..."
                Nerf.saveModel path nerf
  where
    sgdArgs = SGD.SgdArgs
        { SGD.batchSize = batchSize
        , SGD.regVar = regVar
        , SGD.iterNum = iterNum
        , SGD.gain0 = gain0
        , SGD.tau = tau }


exec NER{..} = do
    nerf <- Nerf.loadModel inModel
    case format of
        Text -> do
            inp  <- L.lines <$> L.getContents
            forM_ inp $ \sent -> do
                let forest = Nerf.ner nerf (L.unpack sent)
                    simplify = NETree.mapForest . NETree.onNode $ T.pack . show
                L.putStrLn . showForest $ simplify forest
        XCES -> L.putStrLn
            . XCES2.showXCES
            . (map . map) ( XCES2.nerXCES
                (Nerf.tagset nerf)
                (Nerf.nerX nerf snd) )
            . XCES2.parseXCES
            =<< L.getContents


exec Server{..} = do
    putStr "Loading model..." >> hFlush stdout
    nerf <- Nerf.loadModel inModel
    nerf `seq` putStrLn " done"
    let portNum = N.PortNumber $ fromIntegral port
    putStrLn $ "Listening on port " ++ show port
    S.runNerfServer nerf portNum


exec Client{..} = do
  -- Tagset
  tagsetPath' <- case tagsetPath of
      Nothing -> getDataFileName "config/nkjp-tagset.cfg"
      Just x  -> return x     
  tagset <- P.parseTagset tagsetPath' <$> readFile tagsetPath'
  case format of
    Text -> do
        inp  <- L.lines <$> L.getContents
        forM_ inp $ \sent -> do
            forest <- S.ner host portNum $ L.unpack sent
            let simplify = NETree.mapForest . NETree.onNode $ T.pack . show
            L.putStrLn . showForest $ simplify forest
            -- L.putStrLn (showForest forest)
    XCES -> L.putStrLn
        . XCES2.showXCES
        . (map . map) ( XCES2.nerXCES tagset
            -- TODO: find a safer way to do this!
            (unsafePerformIO . S.nerX host portNum snd) )
        . XCES2.parseXCES
        =<< L.getContents
  where
     portNum = N.PortNumber $ fromIntegral port


exec nerfArgs@Ox{..} = do
    -- Dictionaries
    Resources{..} <- extract nerfArgs
    cfg <- defaultConf
        (catMaybes [poliDict, prolexDict, pnegDict, neLexDict])
        intDict extDict

    -- Tagset
    tagsetPath' <- case tagsetPath of
        Nothing -> getDataFileName "config/nkjp-tagset.cfg"
        Just x  -> return x     
    tagset <- P.parseTagset tagsetPath' <$> readFile tagsetPath'

    -- Format
    let doOx = case format of
            Text    -> Nerf.tryOx
            XCES    -> Nerf.tryOx' tagset

    -- Observation extraction proper
    doOx cfg dataPath


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


exec NKJP2XCES{..} = do
    nkjp <- NKJP.NE.readTrees nkjpPath
    let xces = map (map XCES2.fromNKJP) nkjp
    L.putStrLn $ XCES2.showXCES xces


-- readRaw :: FilePath -> IO [L.Text]
-- readRaw = fmap L.lines . L.readFile


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
