{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parsing the Gazetteer for Polish Named Entities (used formerly within
-- the SProUT platform) in the LMF format.

module NLP.Nerf.Dict.LMF
( parseLmf
, readLmf
) where

import Text.XML.PolySoup
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import NLP.Nerf.Dict.Base

lmfP :: XmlParser L.Text [Entry]
lmfP = true ##> lexEntryP

lexEntryP :: XmlParser L.Text [Entry]
lexEntryP = tag "LexicalEntry" `joinR` do
    many_ $ cut $ tag "feat"
    _words <- many wordP
    sense  <- senseP
    return [Entry x sense | x <- _words]

wordP :: XmlParser L.Text Form
wordP = head <$> (tag "Lemma" <|> tag "WordForm" /> featP "writtenForm")

senseP :: XmlParser L.Text NeType
senseP = head <$> (tag "Sense" //> featP "externalReference" <|> featP "label")

featP :: L.Text -> XmlParser L.Text T.Text
featP x = L.toStrict <$> cut (tag "feat" *> hasAttr "att" x *> getAttr "val")

-- | Parse the dictionary to the list of entries.
parseLmf :: L.Text -> [Entry]
parseLmf = parseXml lmfP

-- | Read the dictionary from the file.
readLmf :: FilePath -> IO [Entry]
readLmf = fmap parseLmf . L.readFile
