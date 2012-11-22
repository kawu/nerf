{-# LANGUAGE OverloadedStrings #-}

module NLP.Nerf.Dict
( preparePoliMorf
, preparePNEG
, prepareNELexicon
, module NLP.Nerf.Dict.Base
) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.PoliMorf as Poli

import NLP.Nerf.Dict.Base
import NLP.Nerf.Dict.PNEG (readPNEG)
import NLP.Nerf.Dict.NELexicon (readNELexicon)

-- | Is it a single word entry?
atomic :: Entry -> Bool
atomic = not . isMultiWord . neOrth

-- | Parse PNEG dictionary and save it in a binary form into
-- the output file.
preparePNEG
    :: FilePath     -- ^ Path to PNEG in the LMF format
    -> FilePath     -- ^ Output file
    -> IO ()
preparePNEG lmfPath outPath = do
    neDict <- fromEntries . filter atomic <$> readPNEG lmfPath
    saveDict outPath neDict

-- | Parse NELexicon, merge it with the PoliMorf and serialize
-- into a binary, DAWG form.
prepareNELexicon
    :: FilePath     -- ^ Path to NELexicon
    -> FilePath     -- ^ Output file
    -> IO ()
prepareNELexicon nePath outPath = do
    neDict <- fromEntries . filter atomic <$> readNELexicon nePath
    saveDict outPath neDict
    -- baseMap <- Poli.mkBaseMap <$> Poli.readPoliMorf poliPath
    -- encodeFile outPath (Poli.merge baseMap neDict)

-- | Parse PoliMorf and extract form/label pairs to construct
-- the dictinoary of NEs.
preparePoliMorf
    :: FilePath     -- ^ File to PoliMorf
    -> FilePath     -- ^ Output file
    -> IO ()
preparePoliMorf poliPath outPath = do
    neDict <- fromPairs . filter (cond . snd)
            . map ((,) <$> Poli.form <*> Poli.cat)
          <$> Poli.readPoliMorf poliPath
    saveDict outPath neDict
  where
    cond x = x /= "pospolita" && x /= ""
