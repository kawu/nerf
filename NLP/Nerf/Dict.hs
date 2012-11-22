{-# LANGUAGE OverloadedStrings #-}

module NLP.Nerf.Dict
( preparePoliMorf
, preparePNEG
, prepareNELexicon
, prepareProlexbase
, preparePNET
, module NLP.Nerf.Dict.Base
) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.PoliMorf as Poli

import NLP.Nerf.Dict.Base
import NLP.Nerf.Dict.PNEG (readPNEG)
import NLP.Nerf.Dict.NELexicon (readNELexicon)
import NLP.Nerf.Dict.Prolexbase (readProlexbase)
import qualified NLP.Nerf.Dict.PNET as PNET

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

-- -- | Parse NELexicon, merge it with the PoliMorf and serialize
-- -- into a binary, DAWG form.
-- | Parse NELexicon dictionary and save it in a binary form.
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
-- the dictionary of NEs.
preparePoliMorf
    :: FilePath     -- ^ Path to PoliMorf
    -> FilePath     -- ^ Output file
    -> IO ()
preparePoliMorf poliPath outPath = do
    neDict <- fromPairs . filter (cond . snd)
            . map ((,) <$> Poli.form <*> Poli.cat)
          <$> Poli.readPoliMorf poliPath
    saveDict outPath neDict
  where
    cond x = x /= "pospolita" && x /= ""

-- | Parse Prolexbase and extract form/label pairs to construct
-- the dictionary.
prepareProlexbase
    :: FilePath     -- ^ Path to Prolexbase
    -> FilePath     -- ^ Output file
    -> IO ()
prepareProlexbase proPath outPath = do
    neDict <- fromEntries . filter atomic <$> readProlexbase proPath
    saveDict outPath neDict

-- | Parse PNET dictionary and save triggers in binary format.
preparePNET
    :: FilePath     -- ^ Path to PNET
    -> FilePath     -- ^ Internal triggers output file
    -> FilePath     -- ^ External triggers output file
    -> IO ()
preparePNET pnetPath intPath extPath = do
    intDict <- mkDict PNET.Internal <$> PNET.readPNET pnetPath
    saveDict intPath intDict
    extDict <- mkDict PNET.External <$> PNET.readPNET pnetPath
    saveDict extPath extDict
  where
    mkDict typ
        = fromPairs
        . filter (not . isMultiWord . fst) 
        . map ((,) <$> PNET.orth <*> PNET.neTyp)
        . filter (PNET.withTyp typ)
