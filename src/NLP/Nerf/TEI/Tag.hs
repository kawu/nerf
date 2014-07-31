{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Suport for tagging TEI corpus.


module NLP.Nerf.TEI.Tag
( tagCorpus
) where


import           Control.Applicative
import           Control.Monad (forM_, replicateM_, forever, when)
import qualified Control.Monad.LazyIO as LazyIO
import           Control.Concurrent
import qualified System.Directory as Dir
import           System.FilePath ((</>))
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L

import qualified Text.NKJP.Morphosyntax as X

import qualified NLP.Nerf.TEI as TEI
import qualified NLP.Nerf as Nerf


--  Communication location.
type PathMVar = MVar (FilePath, FilePath)


-- | Annotate TEI corpus with NEs.  The corpus is a directory tree
-- with `ann_morphosyntax.xml.gz` placed at different levels
-- of the tree. 
tagCorpus
    :: Int              -- ^ Number of Nerf instances to run
    -> Bool             -- ^ Override existing ann_named files?
    -> Nerf.Nerf        -- ^ The model
    -> FilePath         -- ^ Source root directory
    -> FilePath         -- ^ Destination root directory
    -> IO ()
tagCorpus n override nerf srcRoot dstRoot = do
    -- Communication variable
    pathMVar <- newEmptyMVar
    -- Nerf instances
    replicateM_ n $ forkIO $
        runNerf pathMVar override nerf
    -- Walk the directory
    paths <- walkDir srcRoot
    forM_ paths $ \path -> putMVar pathMVar
        ( srcRoot </> path
        , dstRoot </> path )
    -- TODO: this is a dirty trick to not exit before
    -- subthreads finish their job.
    threadDelay $ 1000 * 10^(6::Int)


-- | Run Nerf instance listening on the given MVar for file
-- processing requests.
runNerf :: PathMVar -> Bool -> Nerf.Nerf -> IO ()
runNerf pathMVar override nerf = forever $ do
    (srcRoot, dstRoot) <- takeMVar pathMVar
    let srcPath = srcRoot </> "ann_morphosyntax.xml"
        sgzPath = srcRoot </> "ann_morphosyntax.xml.gz"
        prtPath = dstRoot </> "ann_named.xml.gz.part"
        dstPath = dstRoot </> "ann_named.xml.gz"
    b <- Dir.doesFileExist dstPath
    when (override || not b) $ do
        srcData <- readBytes srcPath
        sgzData <- readBytes sgzPath
        let finData = srcData <|> fmap GZip.decompress sgzData
        just finData $ \morphData -> do
            putStrLn $ "> " ++ dstPath
            Dir.createDirectoryIfMissing True dstRoot
            let morph = X.parseMorph $ L.decodeUtf8 morphData
                tagset = Nerf.tagset nerf
                ner = Nerf.nerX nerf (TEI.toWord tagset . fmap L.toStrict)
                named = map (TEI.nerPara ner) morph
            -- ByteString.writeFile dstPath
            ByteString.writeFile prtPath
                . GZip.compress . L.encodeUtf8
                $ TEI.showTEI named
            Dir.renameFile prtPath dstPath


---------------------
-- Walk directory
---------------------


-- | Walk the given directory tree.  Warning: lazy IO.
walkDir :: FilePath -> IO [FilePath]
walkDir root = doWalk root ""


-- | Walk the given directory tree.  Warning: lazy IO.
doWalk
    :: FilePath     -- ^ Root path
    -> FilePath     -- ^ Path to walk
    -> IO [FilePath]
doWalk root path = do
    b <- Dir.doesDirectoryExist $ root </> path
    if b then ( do
        cs <- listDir $ root </> path
        xs <- concat <$> LazyIO.mapM (doWalk root . (path </>)) cs
        return $ path : xs )
        else return []


-- | List the given directory.
listDir :: FilePath -> IO [FilePath]
listDir path
     =  filter (not . (`elem` [".", ".."]))
    <$> Dir.getDirectoryContents path


---------------------
-- Utils
---------------------


-- | Read ann_morphosyntax.xml file from the given directory, gzipped or not.
readBytes :: FilePath -> IO (Maybe ByteString.ByteString)
readBytes path = do
    b <- Dir.doesFileExist path
    if b
        then Just <$> ByteString.readFile path
        else return Nothing


just :: Maybe a -> (a -> IO ()) -> IO ()
just (Just x) f = f x
just Nothing _ = return ()
