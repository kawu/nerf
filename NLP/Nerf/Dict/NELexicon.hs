-- | Handling the NELexicon dictionary.

module NLP.Nerf.Dict.NELexicon
( parseNELexicon
, readNELexicon
) where

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import NLP.Nerf.Dict.Base

-- | Parse the NELexicon into a list of entries.
parseNELexicon :: L.Text -> [Entry]
parseNELexicon = map parseLine . L.lines

parseLine :: L.Text -> Entry
parseLine line = case L.break (==';') line of
    (_type, _form) -> Entry (L.toStrict $ L.tail _form) (L.toStrict _type)
    _   -> error $ "parseLine: invalid line \"" ++ L.unpack line ++ "\""

-- | Read the dictionary from the file.
readNELexicon :: FilePath -> IO [Entry]
readNELexicon = fmap parseNELexicon . L.readFile
