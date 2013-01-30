{-# LANGUAGE OverloadedStrings #-}

-- | Polish Named Entity Triggers <http://zil.ipipan.waw.pl/PNET> dictionary.

module NLP.Nerf.Dict.PNET
( parsePNET
, readPNET
, Typ (..)
, hasTyp
, Entry (..)
) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

-- | Trigger type.
data Typ
    = Internal
    | External
    deriving (Show, Eq, Ord)

readTyp :: T.Text -> Typ
readTyp "int" = Internal
readTyp "ext" = External
readTyp x     = error $ "readTyp: typ " ++ T.unpack x ++ " unknown"

-- | PNET entry.
data Entry = Entry
    { orth      :: T.Text
    , base      :: T.Text
    , tag       :: T.Text
    , typ       :: Typ
    , neTyp     :: T.Text
    , example   :: T.Text }

-- | Does entry represents a trigger of the given type?
hasTyp :: Typ -> Entry -> Bool
hasTyp x = (==x) . typ

parseLine :: L.Text -> Entry
parseLine line = case map L.toStrict (L.split (=='\t') line) of
    [_orth, _base, _tag, _typ, _neTyp, _example] ->
        Entry _orth _base _tag (readTyp _typ) _neTyp _example
    _   -> error $ "parseLine: invalid row \"" ++ L.unpack line ++ "\""

-- | Parse dictionary into a list of entries.
parsePNET :: L.Text -> [Entry]
parsePNET = map parseLine . L.lines

-- | Read dictionary from the file.
readPNET :: FilePath -> IO [Entry]
readPNET = fmap parsePNET . L.readFile
