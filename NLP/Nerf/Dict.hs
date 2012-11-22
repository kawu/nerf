module NLP.Nerf.Dict
( preparePNEG
, prepareNELexicon
, module NLP.Nerf.Dict.Base
) where

import Control.Applicative ((<$>))
import Data.Binary (encodeFile)
import qualified Data.PoliMorf as Poli

import NLP.Nerf.Dict.Base
import NLP.Nerf.Dict.PNEG (readPNEG)
import NLP.Nerf.Dict.NELexicon (readNELexicon)

-- | Is it a single word entry?
atomic :: Entry -> Bool
atomic = not . isMultiWord . neOrth

-- | Parse the PNEG dictionary and save it in a binary form into
-- the output file.
preparePNEG
    :: FilePath     -- ^ Path to PNEG in the LMF format
    -> FilePath     -- ^ Output file
    -> IO ()
preparePNEG lmfPath outPath = do
    neDict <- mkDict . filter atomic <$> readPNEG lmfPath
    saveDict outPath neDict

-- | Parse the NELexicon, merge it with the PoliMorf and serialize
-- into a binary, DAWG form.
prepareNELexicon
    :: FilePath     -- ^ Path to NELexicon
    -> FilePath     -- ^ Path to PoliMorf
    -> FilePath     -- ^ Output file
    -> IO ()
prepareNELexicon nePath poliPath outPath = do
    neDict  <- mkDict . filter atomic <$> readNELexicon nePath
    baseMap <- Poli.mkBaseMap <$> Poli.readPoliMorf poliPath
    encodeFile outPath (Poli.merge baseMap neDict)
