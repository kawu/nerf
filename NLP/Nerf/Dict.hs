{-# LANGUAGE OverloadedStrings #-}

module NLP.Nerf.Dict
( extractPoliMorf
, extractPNEG
, extractNELexicon
, extractProlexbase
, extractIntTriggers
, extractExtTriggers
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

-- | Extract NEs dictionary from PNEG.
extractPNEG
    :: FilePath     -- ^ Path to PNEG in the LMF format
    -> IO Dict
extractPNEG lmfPath =
    fromEntries . filter atomic <$> readPNEG lmfPath

-- | Extract NEs dictionary from NELexicon.
extractNELexicon
    :: FilePath     -- ^ Path to NELexicon
    -> IO Dict
extractNELexicon nePath =
    fromEntries . filter atomic <$> readNELexicon nePath

-- | Extract NEs dictionary from PoliMorf.
extractPoliMorf
    :: FilePath     -- ^ Path to PoliMorf
    -> IO Dict
extractPoliMorf poliPath
    = fromPairs . filter (cond . snd)
    . map ((,) <$> Poli.form <*> Poli.cat)
    <$> Poli.readPoliMorf poliPath
  where
    cond x = x /= "pospolita" && x /= ""

-- | Extract NEs dictionary from Prolexbase.
extractProlexbase
    :: FilePath     -- ^ Path to Prolexbase
    -> IO Dict
extractProlexbase proPath = do
    fromEntries . filter atomic <$> readProlexbase proPath

-- | Extract internal triggers from PNET dictionary.
extractIntTriggers
    :: FilePath     -- ^ Path to PNET
    -> IO Dict
extractIntTriggers pnetPath =
    mkTriggers PNET.Internal <$> PNET.readPNET pnetPath

-- | Extract external triggers from PNET dictionary.
extractExtTriggers
    :: FilePath     -- ^ Path to PNET
    -> IO Dict
extractExtTriggers pnetPath =
    mkTriggers PNET.External <$> PNET.readPNET pnetPath

mkTriggers :: PNET.Typ -> [PNET.Entry] -> Dict
mkTriggers typ
    = fromPairs
    . filter (not . isMultiWord . fst) 
    . map ((,) <$> PNET.orth <*> PNET.neTyp)
    . filter (PNET.withTyp typ)
