{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- | Support for the XCES format.
-- * Prety print.


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


-- | An XML tag.
type Tag = S.Tag L.Text


-- | An XML parser.
type Parser a = XmlParser L.Text a


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
-- XML sentence
---------------------------------------------------------------------


-- | An XML sentence.
type Sent = [Tok]


-- | A sentence opening tag.
sentOpen :: Tag
sentOpen = S.TagOpen "chunk" [("type", "s")]


-- | A sentence opening tag.
sentClose :: Tag
sentClose = S.TagClose "chunk"


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
joinNps (_:xs)                  = joinNps xs    -- Exception: two <ns> tags


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
neOpen x = S.TagOpen "group" [("type", L.fromStrict x)]


-- | A sentence opening tag.
neClose :: Tag
neClose = S.TagClose "group"


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
tokOpen = S.TagOpen "tok" []


-- | A sentence opening tag.
tokClose :: Tag
tokClose = S.TagClose "tok"


-- | Assumption: orth is the first element of the token description.
tokP :: Parser Tok
tokP = tag "tok" ^> Tok
    <$> (tag "orth" ^> text)
    <*> pure False
    <*> collTags


-- | Render token.
-- TODO: Split nps tag.
renderTok :: Tok -> [Tag]
renderTok Tok{..} =
    nsTag ++ between tokOpen tokClose (orthTag ++ other)
  where
    orthTag = [S.TagOpen "orth" [], S.TagText orth, S.TagClose "orth"]
    nsTag   = if nps
--         then [S.TagOpen "ns" [], S.TagClose "ns"]
        then [S.TagClose "ns"]
        else []


---------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------


-- | Put the list between the two elements.
between :: a -> a -> [a] -> [a]
between p q xs = p : xs ++ [q]


-- -- | A sentence opening tag.
-- lexOpen :: Tag
-- lexOpen = S.TagOpen "lex" []
-- 
-- 
-- -- | A sentence opening tag.
-- lexClose :: Tag
-- lexClose = S.TagClose "lex"


-- | Add newlines etc.
pretify :: [Tag] -> [Tag]
pretify = go where
    go (x:xs)
        | x ~== lexOpen = x : go' xs
        | otherwise     = x : nl : go xs
    go []               = []
    go' (x:xs)
        | x == lexClose = x : nl : go xs
        | otherwise     = x : go' xs
    go' []              = []
    nl = S.TagText "\n"


-- -- | Add newlines etc.
-- pretify :: [Tag] -> [Tag]
-- pretify = go where
--     go (x:xs)
--         | x ~== lexOpen = x : go' xs
--         | otherwise     = x : nl : go xs
--     go []               = []
--     go' (x:xs)
--         | x == lexClose = x : nl : go xs
--         | otherwise     = x : go' xs
--     go' []              = []
--     nl = S.TagText "\n"


-- -- | Add newlines etc.
-- pretify :: Int -> [Tag] -> [Tag]
-- pretify n = go where
-- 
--     -- Outside of a sentence
--     go (x:xs)
--         | x == sentOpen     = x : nl : go' 0 xs
--         | otherwise         = x : nl : go xs
--     go []                   = []
-- 
--     -- Inside of a sentence
--     go' k (x:xs)
--         | x == sentClose    = x : nlIf k        ++ go xs
--         | S.isTagOpen x     = x : nlIf (k+1)    ++ go' (k+1) xs
--         | S.isTagClose x    = x : nlIf (k-1)    ++ go' (k-1) xs
--         | otherwise         = x : nlIf k        ++ go' k xs
--     go' _ []                = []
-- 
--     -- Newline tag
--     nlIf k
--         | k < n     = [nl]
--         | otherwise = []
--     nl = S.TagText "\n"
        


---------------------------------------------------------------------
-- Annotation
---------------------------------------------------------------------


-- | Annotate XCES (in a form of a tag list) with NEs.
nerXCES :: Nerf.Nerf -> L.Text -> L.Text
nerXCES nerf
    = S.renderTagsOptions opts
    . pretify . unChunk
    . mapR
        ( renderNeForest
        . nerSent nerf
        . parseSent )
    . chunk . parseTags
  where
    mapR = map . fmap
    opts = S.renderOptions {S.optMinimize = const True}


-- | Annotate XCES sentence with NEs.
nerSent :: Nerf.Nerf -> Sent -> NeForest NE Tok
nerSent _ xs = [Node (Right x) [] | x <- xs]
-- nerSent nerf sent = Tok.moveNEs
--     (Nerf.ner nerf $ restoreOrigSent sent)
--     sent


-- | Restore original sentence.
restoreOrigSent :: Sent -> String
restoreOrigSent 
    = dropWhile isSpace
    . concatMap tokStr
  where
    tokStr Tok{..} = (if nps then "" else " ") ++ (L.unpack orth)
