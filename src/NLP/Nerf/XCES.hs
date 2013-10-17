{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module NLP.Nerf.XCES
(
-- * Types
  Sent
, Tok (..)
) where


import qualified Data.Text.Lazy as L
import           Text.HTML.TagSoup hiding (Tag)
import qualified Text.HTML.TagSoup as TagSoup
import           Text.XML.PolySoup hiding (Parser)

import           Data.Named.Tree
import           NLP.Nerf.Types
import qualified NLP.Nerf.Tokenize as Tok


-- We assume a rigid structure within the sentence <chunk type="s"> tags.
--
-- As a first step, we need to translate the "tag soup" into a
-- [Either Tag XML] list, where `Right` elements represent XML
-- tree fragments corresponding to individual sentences and `Left`
-- elements corresopnd to the other tags, which are not a part of
-- any sentence.
--
-- After that we can NER individual sentences (XML -> XML), parse them
-- (XML -> [Tag]), join all tags ([Either Tag [Tag] -> [Tag]) and render
-- the entire list of tags.  Voila!


-- | An XML tag.
type Tag = TagSoup.Tag L.Text


-- | An XML parser.
type Parser a = XmlParser L.Text a


-- | Group tags corresponding to individual sentences as right elements.
chunk :: [Tag] -> [Either Tag [Tag]]
chunk (x:xs)
    | x == sentOpen =
        let (sent, rest) = takeSent xs
        in  Right (x:sent) : chunk rest
    | otherwise = Left x : chunk xs


-- | Take tags starting with a sentence.
takeSent :: [Tag] -> ([Tag], [Tag])
takeSent = go [] where
    go acc (x:xs)
        | x == sentClose    = (reverse $ x:acc, xs)
        | otherwise         = go (x:acc) xs


---------------------------------------------------------------------
-- XML sentence
---------------------------------------------------------------------


-- | An XML sentence.
type Sent = [Tok]


-- | A sentence opening tag.
sentOpen :: Tag
sentOpen = TagOpen "chunk" [("type", "s")]


-- | A sentence opening tag.
sentClose :: Tag
sentClose = TagClose "chunk"


-- | XML sentence parser.
sentP :: Parser Sent
sentP = fmap joinNps
    $ (tag "chunk" *> hasAttr "type" "s") />
        (Left <$> tokP <|> Right <$> nsP)


-- | No-space parser.
nsP :: Parser ()
nsP = cut $ tag "ns"


-- | Join nps information with tokens.
joinNps :: [Either Tok ()] -> [Tok]
joinNps (Right _ : Left t : xs) = t { nps = True }  : joinNps xs
joinNps (Left t : xs)           = t { nps = False } : joinNps xs
joinNps []                      = []
joinNps (x:xs)                  = joinNps xs    -- Exception: two <ns> tags


-- | Parse a list of tags into a sentence.
parseSent :: [Tag] -> Sent
parseSent = tagsParseXml sentP


-- -- | Render a sentence.
-- renderSent :: Sent -> [Tag]
-- renderSent ts = sentOpen : concatMap renderTok ts ++ [sentClose]


---------------------------------------------------------------------
-- Annotated XML sentence
---------------------------------------------------------------------


-- | A sentence opening tag.
neOpen :: NE -> Tag
neOpen x = TagOpen "group" [("type", L.fromStrict x)]


-- | A sentence opening tag.
neClose :: Tag
neClose = TagClose "group"


-- | Render an annotated sentence.
renderNeForest :: NeForest NE Tok -> [Tag]
renderNeForest = between sentOpen sentClose . concatMap renderNeTree


-- | Render an element of an annotated sentence.
renderNeTree :: NeTree NE Tok -> [Tag]
renderNeTree (Node (Left v) xs) = between (neOpen v) neClose
                                $ concatMap renderNeTree xs
renderNeTree (Node (Right t) _) = renderTok t


---------------------------------------------------------------------
-- XML Token
---------------------------------------------------------------------


-- | An XML sentence.
data Tok = Tok
    { orth  :: L.Text
    , nps   :: Bool     -- ^ No preceding space
    , other :: [Tag] }


instance Tok.Word Tok where
    word = Tok.word . orth


-- | A sentence opening tag.
tokOpen :: Tag
tokOpen = TagOpen "tok" []


-- | A sentence opening tag.
tokClose :: Tag
tokClose = TagClose "tok"


-- | Assumption: orth is the first element of the token description.
tokP :: Parser Tok
tokP = tag "tok" ^> Tok
    <$> (tag "orth" ^> text)
    <*> pure False
    <*> collTags


-- | Render token.
renderTok :: Tok -> [Tag]
renderTok Tok{..} = between tokOpen tokClose other


---------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------


-- | Put the list between the two elements.
between :: a -> a -> [a] -> [a]
between p q xs = p : xs ++ [q]


---------------------------------------------------------------------
-- Move out
---------------------------------------------------------------------


-- annSent :: Nerf.Nerf -> Sent -> Sent
