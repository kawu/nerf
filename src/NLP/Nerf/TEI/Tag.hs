{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Suport for tagging TEI corpus.


module NLP.Nerf.TEI.Tag
( tagCorpus
) where


import           Control.Applicative
import           Control.Monad (forM_, replicateM_, forever)
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


-- -- | Annotate TEI corpus with NEs.  The corpus is a directory tree
-- -- with `ann_morphosyntax.xml.gz` placed at different levels
-- -- of the tree. 
-- tagCorpus
--     :: Nerf.Nerf        -- ^ The model
--     -> FilePath         -- ^ Source root directory
--     -> FilePath         -- ^ Destination root directory
--     -> IO ()
-- tagCorpus nerf srcRoot dstRoot = do
--     paths <- walkDir srcRoot
--     forM_ paths $ \path -> do
--         let srcPath = srcRoot </> path </> "ann_morphosyntax.xml"
--             sgzPath = srcRoot </> path </> "ann_morphosyntax.xml.gz"
--             dstPath = dstRoot </> path </> "ann_named.xml.gz"
--         srcData <- readBytes srcPath
--         sgzData <- readBytes sgzPath
--         let finData = srcData <|> fmap GZip.decompress sgzData
--         just finData $ \morphData -> do
--             putStrLn $ "> " ++ dstPath
--             Dir.createDirectoryIfMissing True $ dstRoot </> path
--             let morph = X.parseMorph $ L.decodeUtf8 morphData
--                 tagset = Nerf.tagset nerf
--                 ner = Nerf.nerX nerf (TEI.toWord tagset . fmap L.toStrict)
--                 named = map (TEI.nerPara ner) morph
--             ByteString.writeFile dstPath
--                 . GZip.compress . L.encodeUtf8
--                 $ TEI.showTEI named


--  Communication location.
type PathMVar = MVar (FilePath, FilePath)


-- | Annotate TEI corpus with NEs.  The corpus is a directory tree
-- with `ann_morphosyntax.xml.gz` placed at different levels
-- of the tree. 
tagCorpus
    :: Int              -- ^ Number of Nerf instances to run
    -> Nerf.Nerf        -- ^ The model
    -> FilePath         -- ^ Source root directory
    -> FilePath         -- ^ Destination root directory
    -> IO ()
tagCorpus n nerf srcRoot dstRoot = do
    -- Communication variable
    pathMVar <- newEmptyMVar
    -- Nerf instances
    replicateM_ n $ forkIO $
        runNerf pathMVar nerf
    -- Walk the directory
    paths <- walkDir srcRoot
    forM_ paths $ \path -> putMVar pathMVar
        ( srcRoot </> path
        , dstRoot </> path )


-- | Run Nerf instance listening on the given MVar for file
-- processing requests.
runNerf :: PathMVar -> Nerf.Nerf -> IO ()
runNerf pathMVar nerf = forever $ do
    (srcRoot, dstRoot) <- takeMVar pathMVar
    let srcPath = srcRoot </> "ann_morphosyntax.xml"
        sgzPath = srcRoot </> "ann_morphosyntax.xml.gz"
        dstPath = dstRoot </> "ann_named.xml.gz"
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
        ByteString.writeFile dstPath
            . GZip.compress . L.encodeUtf8
            $ TEI.showTEI named


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


