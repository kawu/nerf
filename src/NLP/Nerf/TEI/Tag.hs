{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Suport for tagging TEI corpus.


module NLP.Nerf.TEI.Tag
( tagCorpus
) where


import           Control.Applicative
import           Control.Monad (when, forM_)
import qualified Control.Monad.LazyIO as LazyIO
import qualified System.Directory as Dir
import           System.FilePath ((</>))
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L

import qualified Text.NKJP.Morphosyntax as X

import qualified NLP.Nerf.TEI as TEI
import qualified NLP.Nerf as Nerf


-- | Annotate TEI corpus with NEs.  The corpus is a directory tree
-- with `ann_morphosyntax.xml.gz` placed at different levels
-- of the tree. 
tagCorpus
    :: Nerf.Nerf        -- ^ The model
    -> FilePath         -- ^ Source root directory
    -> FilePath         -- ^ Destination root directory
    -> IO ()
tagCorpus nerf srcRoot dstRoot = do
    paths <- walkDir srcRoot
    forM_ paths $ \path -> do
        let srcName = "ann_morphosyntax.xml.gz"
            dstName = "ann_named.xml.gz"
            srcPath = srcRoot </> path </> srcName
            dstPath = dstRoot </> path </> dstName
        putStrLn srcPath
        putStrLn dstPath
        b <- Dir.doesFileExist srcPath
        when b $ do
            putStrLn $ "> " ++ dstPath
            morph <- X.parseMorph . L.decodeUtf8 . GZip.decompress
                 <$> ByteString.readFile srcPath
            let tagset = Nerf.tagset nerf
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
