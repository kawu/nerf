{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- | Support for the XCES format.


module NLP.Nerf.XCES
( nerXCES
) where


import qualified Data.Text.Lazy as L
import           Data.Char (isSpace)
import           Text.HTML.TagSoup ((~==))
import qualified Text.HTML.TagSoup as S
import           Text.XML.PolySoup hiding (Parser)

import           Data.Named.Tree
import           NLP.Nerf.Types
import qualified NLP.Nerf.Tokenize as Tok
import qualified NLP.Nerf as Nerf


---------------------------------------------------------------------
-- Core types
---------------------------------------------------------------------


-- | An XML tag.
type Tag = S.Tag L.Text


-- | An XML parser.
type Parser a = XmlParser L.Text a


---------------------------------------------------------------------
-- XML tags
---------------------------------------------------------------------


-- | A sentence opening tag.
sentOpen :: Tag
sentOpen = S.TagOpen "chunk" [("type", "s")]


-- | A sentence opening tag.
sentClose :: Tag
sentClose = S.TagClose "chunk"


-- | A sentence opening tag.
tokOpen :: Tag
tokOpen = S.TagOpen "tok" []


-- | A sentence opening tag.
tokClose :: Tag
tokClose = S.TagClose "tok"



---------------------------------------------------------------------
-- XML chunking
---------------------------------------------------------------------


-- | Group tags corresponding to individual sentences as right elements.
chunk :: [Tag] -> [Either Tag [Tag]]
chunk (x:xs)
    | x ~== sentOpen =
        let (sent, rest) = takeSent xs
        in  Right (x:sent) : chunk rest
    | otherwise = Left x : chunk xs
chunk [] = []


-- | Take tags starting with a sentence.
takeSent :: [Tag] -> ([Tag], [Tag])
takeSent = go [] where
    go acc (x:xs)
        | x == sentClose    = (reverse $ x:acc, xs)
        | otherwise         = go (x:acc) xs
    go _ []                 = error "takeSent: expected sentence closing tag"


-- | Remove division into chunks.
unChunk :: [Either Tag [Tag]] -> [Tag]
unChunk = concatMap $ either (:[]) id


---------------------------------------------------------------------
-- XML sentence intermediate
---------------------------------------------------------------------


-- | Intermediate sentence representation.
data SentI = SentI {
    -- | Beginning tag.
      sentBegI  :: Tag
    -- | Sentence contents.
    , sentConI  :: [(SegT, [Tag])] }


-- | Type of a sentence sub-element.
data SegT = TokT | NsT | OtherT


-- | Identify type of a sub-element.
-- TODO: It can be easily optimized, since we know that
-- input tags already reprensent an XML tree.
-- There's no need to use `tagsParseXml` here.
idSegT :: [Tag] -> SegT
idSegT = tagsParseXml $
        TokT <$ cut (tag "tok")
    <|> NsT  <$ cut (tag "ns")
    <|> pure OtherT


-- | XML intermediate sentence parser.
sentIP :: Parser SentI
sentIP =
    begP >^> \x -> SentI x <$> many elemP
  where
    begP = tag "chunk" *> hasAttr "type" "s" *> getTag
    elemP = (\x -> (idSegT x, x)) <$> elemTags


---------------------------------------------------------------------
-- XML sentence
---------------------------------------------------------------------


-- | An XML sentence.
data Sent t = Sent {
      sentBeg   :: Tag
    , sentCon   :: t Tok
    , sentEnd   :: [Tag] }


-- | Translate sentence into its final representation.
joinSent :: SentI -> Sent []
joinSent SentI{..} =
    uncurry (Sent sentBegI) (go [] [] False sentConI)
  where
    -- TODO: could we represent this function as a fold?
    go acc res hasNs ((typ, tags) : xs) = case typ of
        TokT ->
            let tok = Tok
                    { orth   = tagsParseXml tokOrthP tags
                    , nps    = hasNs
                    , tagsIn = tags
                    , tagsBf = flatRev acc }
            in  go [] (tok:res) False xs
        NsT    -> go (tags:acc) res True xs
        OtherT -> go (tags:acc) res hasNs xs
    go acc res _ [] = (reverse res, flatRev acc)
    flatRev = concat . reverse


-- | Parse a list of tags into a sentence.
parseSent :: [Tag] -> Sent []
parseSent = joinSent . tagsParseXml sentIP


---------------------------------------------------------------------
-- Annotated XML sentence
---------------------------------------------------------------------


-- | List of a elements annotated with NEs.
newtype Ann a = Ann { unAnn :: NeForest NE a }


-- | A sentence opening tag.
neOpen :: NE -> Tag
neOpen x = S.TagOpen "group" [("type", L.fromStrict x)]


-- | A sentence opening tag.
neClose :: Tag
neClose = S.TagClose "group"


-- | Render an annotated sentence.
renderAnnSent :: Sent Ann -> [Tag]
renderAnnSent Sent{..}
    = sentBeg
    : concatMap renderNeTree (unAnn sentCon)
    ++ sentEnd


-- | Render an element of an annotated sentence.
renderNeTree :: NeTree NE Tok -> [Tag]
renderNeTree (Node (Left v) xs) = between (neOpen v) neClose
                                $ concatMap renderNeTree xs
renderNeTree (Node (Right t) _) = renderTok t


---------------------------------------------------------------------
-- XML Token
---------------------------------------------------------------------


-- | An XML token.
data Tok = Tok
    { orth      :: L.Text   -- ^ Orthographic form
    , nps       :: Bool     -- ^ No preceding space
    , tagsIn    :: [Tag]    -- ^ Token tags 
    , tagsBf    :: [Tag]    -- ^ Non-token tags before the token
    }


instance Tok.Word Tok where
    word = Tok.word . orth


tokOrthP :: Parser L.Text
tokOrthP = maybe "" id <$> (tag "tok" ^> findIgnore (tag "orth" ^> text))


-- | Render token.
renderTok :: Tok -> [Tag]
renderTok Tok{..} = tagsBf ++ tagsIn


---------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------


-- | Put the list between the two elements.
between :: a -> a -> [a] -> [a]
between p q xs = p : xs ++ [q]


---------------------------------------------------------------------
-- Annotating
---------------------------------------------------------------------


-- | Annotate XCES (in a form of a tag list) with NEs.
nerXCES :: Nerf.Nerf -> L.Text -> L.Text
nerXCES nerf
    = S.renderTags
    . unChunk
    . mapR
        ( renderAnnSent
        . nerSent nerf
        . parseSent )
    . chunk . parseTags
  where
    mapR = map . fmap
    -- opts = S.renderOptions {S.optMinimize = const True}


-- | Annotate XCES sentence with NEs.
nerSent :: Nerf.Nerf -> Sent [] -> Sent Ann
nerSent _ s = s
    { sentCon = Ann [Node (Right x) [] | x <- sentCon s] }

-- nerSent nerf sent = Tok.moveNEs
--     (Nerf.ner nerf $ restoreOrigSent sent)
--     sent


-- -- | Restore original sentence.
-- restoreOrigSent :: Sent -> String
-- restoreOrigSent 
--     = dropWhile isSpace
--     . concatMap tokStr
--   where
--     tokStr Tok{..} = (if nps then "" else " ") ++ (L.unpack orth)
