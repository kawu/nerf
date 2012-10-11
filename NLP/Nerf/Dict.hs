module NLP.Nerf.Dict
( prepareLMF
, prepareNeLexicon
) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Data.Binary (encodeFile)
import qualified Data.PoliMorf as Poli
import qualified Data.Map as M
import qualified Data.Text as T

import NLP.Nerf.Dict.Base
import NLP.Nerf.Dict.LMF (readLmf)
import NLP.Nerf.Dict.NELexicon (readNeLexicon)
import qualified NLP.Adict.Trie as Trie

-- | Make dictionary consisting only from one word NEs.
mkDictW1 :: [Entry] -> NeDict
mkDictW1 =
    let oneWord x _ = not (isMultiWord x)
    in  siftDict oneWord . mkDict

-- | Parse the LMF dictionary and save it in a binary form into
-- the output file.
prepareLMF
    :: FilePath     -- ^ Path to LMF
    -> FilePath     -- ^ Output file
    -> IO ()
prepareLMF lmfPath outPath = do
    neDict <- mkDictW1 <$> readLmf lmfPath
    saveDict outPath neDict

-- | Parse the NELexicon, merge it with the PoliMorf and serialize
-- into a binary, DAWG form.
prepareNeLexicon
    :: FilePath     -- ^ Path to NELexicon
    -> FilePath     -- ^ Path to PoliMorf
    -> FilePath     -- ^ Output file
    -> IO ()
prepareNeLexicon nePath poliPath outPath = do
    neDict  <- mkDictW1 <$> readNeLexicon nePath
    baseMap <- Poli.mkBaseMap <$> Poli.readPoliMorf poliPath
    let neDict' = Poli.merge baseMap neDict
        trie    = Trie.fromList $ map (first T.unpack) (M.assocs neDict')
    encodeFile outPath (Trie.serialize trie)
