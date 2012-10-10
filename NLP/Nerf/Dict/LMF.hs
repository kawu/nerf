{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parsing the Gazetteer for Polish Named Entities (used formerly within
-- the SProUT platform) in the LMF format.

module NLP.Nerf.Dict.LMF
( 
-- * LMF entry
  Form
, NeType
, Entry (..)

-- * Parsing LMF
, parseLmf
, readLmf

-- * Dictionary
, NeDict
, mkDict
, saveDict
, loadDict
) where

import Text.XML.PolySoup
import Data.Binary (encodeFile, decodeFile)
import Data.Text.Binary ()
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

-- | A orthographic form.
type Form = T.Text

-- | A type of named entity.
type NeType = T.Text

-- | A Named Entity entry from the LMF dictionary.
data Entry = Entry
    { neOrth :: !Form	-- ^ Orthographic form of the NE
    , neType :: !NeType -- ^ Type of the NE
    } deriving (Show, Read, Eq, Ord)

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

-- | A NeDict is a map from forms to NE types.  Each NE may be annotated
-- with multiple types.
type NeDict = M.Map Form (S.Set NeType)

-- | Construct the dictionaru from the list of entries.
mkDict :: [Entry] -> NeDict
mkDict xs = M.fromListWith S.union
    [(neOrth x, S.singleton $ neType x) | x <- xs]

-- | Save the dictionary in the file.
saveDict :: FilePath -> NeDict -> IO ()
saveDict = encodeFile

-- | Load the dictionary from the file.
loadDict :: FilePath -> IO NeDict
loadDict = decodeFile
