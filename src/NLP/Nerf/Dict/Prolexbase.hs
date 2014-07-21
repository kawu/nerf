-- | Handling Prolexbase dictionaries, both with the
-- same storage format.

module NLP.Nerf.Dict.Prolexbase
( parseProlexbase
, readProlexbase
) where

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import           NLP.Nerf.Dict.Base

-- | Parse dictionary into a list of entries.
parseProlexbase :: L.Text -> [Entry]
parseProlexbase = map parseLine . L.lines

parseLine :: L.Text -> Entry
parseLine row = case map L.toStrict (L.split (=='\t') row) of
    [_form, _base, _tag, _cat] -> Entry _form _cat
    _   -> error $ "parseLine: invalid row \"" ++ L.unpack row ++ "\""

-- | Read the dictionary from the file.
readProlexbase :: FilePath -> IO [Entry]
readProlexbase = fmap parseProlexbase . L.readFile
