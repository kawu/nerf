{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}


-- | Support for the XCES format (new version).
--
-- This is not a regular XCES data format, it provides a way to
-- represent NEs as well.


module NLP.Nerf.XCES2
( 
-- * Data types
  XCES
, Para
, Sent
, NE (..)
, Tok (..)
, Lex (..)

-- * Parsing
, parseXCES

-- * Printing
, showXCES

-- * NKJP
, fromNKJP
) where


import           Control.Applicative
-- import           Control.Monad (void)
import           Control.Arrow (second)
import           Data.String (IsString)
import           Data.List (intersperse)
import qualified Data.Foldable as F
import qualified Data.Text.Lazy as L

-- import           Data.Tree
-- import qualified Data.Named.Tree as Tr
-- import           Data.Named.Tree (NeForest)
import           Data.Named.Tree

import           Text.StringLike (StringLike)
import           Text.HTML.TagSoup (Tag (..))
import qualified Text.HTML.TagSoup as TS

import           Text.XML.PolySoup hiding (P, Q)
import qualified Text.XML.PolySoup as P

-- For the sake of `fromNKJP` function.
import qualified Text.NKJP.Named as NKJP.NE
import qualified Text.NKJP.Morphosyntax as NKJP.MX


---------------------------------------------------------------------
-- XCES data types
---------------------------------------------------------------------


-- | XCES file consists of a list of paragraphs.
type XCES t = [Para t]


-- | Each paragraph consists of a list of sentences (there is no
-- paragraph identifier).
type Para t = [Sent t]


-- | A sentence is a forest of `NE`s (kept in internal nodes)
-- with tokens (or no-space markers) stored in leaves.
-- No-space markers are represented with `Nothing` values.
--
-- `Sent t` is equal to `NeForest (NE t) (Maybe (Tok t))` as well.
type Sent t = [Root t]


-- | A root of the NE forest.
type Root t = NeTree (NE t) (Maybe (Tok t))


-- | A named entity (NE).  It is a higly simplified version of
-- the definition given in the nkjp package.
data NE t = NE
    { neType        :: t
    , subType       :: Maybe t
    , derivType     :: Maybe t }
    deriving (Show, Functor)


-- | NE definition given in the nkjp package:
-- data NE t = NE
--     { neID          :: t
--     -- | We don't specify `derivFrom` for the same reason for which
--     -- we don't specify `base` values. 
--     , derived       :: Maybe (Deriv t)
--     , neType        :: t
--     , subType       :: Maybe t
--     -- | Can be easly calculated from tokens. 
--     , orth          :: t
--     -- | Left base or Right when.  Nerf doesn't output base
--     -- forms, for which reason they are ignored.
--     , base          :: Maybe (Either t t)
--     -- | The rest of attributes are irrelevant from Nerf and
--     -- full NKJP points of view.
--     , cert          :: Maybe Cert
--     , certComment   :: Maybe t
--     , ptrs          :: [Ptr t] }
--     deriving (Show)
-- 
-- -- | If NE is a derived entity.
-- data Deriv t = Deriv
--     { derivType :: t 
--     , derivFrom :: t }
--     deriving (Show, Functor)


-- | A token.
data Tok t = Tok
    { orth  :: t
    , lexs  :: [Lex t] }
    deriving (Show, Functor)


-- | A lexical entry -- a potential interpretation of a segment.
data Lex t = Lex
    { base      :: t
    , ctag      :: t
    -- | Was it chosen while disambiguating?
    , disamb    :: Bool }
    deriving (Show, Functor)


---------------------------------------------------------------------
-- Configuration (attributes not present in standard XCSE format)
---------------------------------------------------------------------


-- | NE tag name.
neS :: IsString s => s
neS = "group"


-- | NE type.
neTypeS :: IsString s => s
neTypeS = "type"


-- | NE subtype.
neSubTypeS :: IsString s => s
neSubTypeS = "subtype"


-- | NE derivation type.
neDerivTypeS :: IsString s => s
neDerivTypeS = "derivtype"


-------------------------------------------------
-- Parsing
-------------------------------------------------


-- TODO: Change L.Text to (Eq t).


-- | Parse textual contents of the ann_morphosyntax.xml file.
parseXCES :: L.Text -> XCES L.Text
parseXCES = F.concat . evalP xcesP . parseForest . TS.parseTags


-- | Parsing predicates.
type P a = P.P (XmlTree L.Text) a
type Q a = P.Q (XmlTree L.Text) a


xcesP :: P (XCES L.Text)
xcesP = concat <$> every' (true //> paraQ)


paraQ :: Q (Para L.Text)
paraQ = named "chunk" *> hasAttrVal "type" "p" /> sentQ


sentQ :: Q (Sent L.Text)
sentQ = named "chunk" *> hasAttrVal "type" "s" /> nodeQ


nodeQ :: Q (Root L.Text)
nodeQ = (flip Node [] . Right <$> segQ) <|> neTreeQ


segQ :: Q (Maybe (Tok L.Text))
segQ =  (Just <$> tokQ)
    <|> (Nothing <$ nsQ)
    where nsQ = node $ named "ns"


tokQ :: Q (Tok L.Text)
tokQ = named "tok" `joinR` (Tok
    <$> first (getTxt "orth")
    <*> every' lexQ )


lexQ :: Q (Lex L.Text)
lexQ = (getAttr "disamb" <$> named "lex") `join` \dmb -> ( Lex
    <$> first (getTxt "base")
    <*> first (getTxt "ctag")
    <*> pure (dmb == Just "1") )


neTreeQ :: Q (Root L.Text)
neTreeQ =
    mkNode <$> (neQ </> nodeQ)
  where
    mkNode (ne, xs) = Node (Left ne) xs
    neQ = named neS *> ( NE
        <$> attr neTypeS
        <*> optional (attr neSubTypeS)
        <*> optional (attr neDerivTypeS) )


-------------------------------------------------
-- Parsing utilities
-------------------------------------------------


-- | Get contexnts of the embedded text XML node.
getTxt :: L.Text -> Q L.Text
getTxt x = named x `joinR` first (node text)


-------------------------------------------------
-- Printing
-------------------------------------------------


-- TODO: add a more generic solution for the
-- `sperse breakLine` trick.


-- | Convert XCES into its XML representation. 
showXCES :: (IsString s, StringLike s) => XCES s -> s
showXCES =
    let opts = TS.renderOptions { TS.optMinimize = const True }
    in  TS.renderTagsOptions opts . renderTree . xcesToXML


-- | Convert XCES to XML tree.
xcesToXML :: IsString s => XCES s -> Tree (Tag s)
xcesToXML xs = Node
    (TagOpen "chunkList" [])
    (sperse breakLine $ map paraToXML xs)


paraToXML :: IsString s => Para s -> Tree (Tag s)
paraToXML xs = Node
    (TagOpen "chunk" [("type", "p")])
    (sperse breakLine $ map sentToXML xs)


sentToXML :: IsString s => Sent s -> Tree (Tag s)
sentToXML xs = Node
    (TagOpen "chunk" [("type", "s")])
    (sperse breakLine $ map rootToXML xs)


-- rootToXML :: IsString s => Root s -> Tree (Tag s)
-- rootToXML =
--     mapTree $ either id id . onEither f g
--   where
--     f NE{..} = TagOpen "ne"
--         [ ("type", neType) ]
--         -- , ("subtype", subType)
--         -- , ("derivtype", derivType) ]
--     g Nothing = TagOpen "ns" []
--     g (Just Tok{..}) = TagText "ASDF"


rootToXML :: IsString s => Root s -> Tree (Tag s)
rootToXML (Node r xs) = case r of
    Left NE{..} -> Node neTag
        (sperse breakLine $ map rootToXML xs)
      where
        neTag = TagOpen neS $
            [ (neTypeS, neType) ] ++
            mbTag neSubTypeS subType ++
            mbTag neDerivTypeS derivType
        mbTag x y = case y of
            Nothing -> []
            Just z  -> [(x, z)]
    Right Nothing -> Node (TagOpen "ns" []) []
    Right (Just tok) -> tokToXML tok
    

tokToXML :: IsString s => Tok s -> Tree (Tag s)
tokToXML Tok{..} = mkNode "token" []
    $ sperse breakLine
    $ mkNode "orth" [] [mkText orth]
    : map lexToXML lexs


lexToXML :: IsString s => Lex s -> Tree (Tag s)
lexToXML Lex{..} = mkNode "lex" atts
    [ mkNode "base" [] [mkText base]
    , mkNode "ctag" [] [mkText ctag] ]
  where
    atts = if disamb
        then [("disamb", "1")]
        else []


-------------------------------------------------
-- Printing utilities
-------------------------------------------------


mkNode :: IsString s => s -> [TS.Attribute s] -> Forest (Tag s) -> Tree (Tag s)
mkNode x = Node . TagOpen x


mkText :: s -> Tree (Tag s)
mkText x = Node (TagText x) []


breakLine :: IsString s => Tree (Tag s)
breakLine = mkText "\n"


-- | Similar to `intersperse`, but adds the item at the beginning
-- and at the end of the list as well.
sperse :: a -> [a] -> [a]
sperse y (x:xs) = y : x : sperse y xs 
sperse y [] = [y]


-------------------------------------------------
-- NKJP
-------------------------------------------------


-- | Construct `Root t` from a NKJP sentence.
-- TODO: include <ns> markers.
fromNKJP
    :: NeForest (NKJP.NE.NE L.Text) (NKJP.MX.Seg L.Text)
    -> NeForest (NE L.Text) (Maybe (Tok L.Text))
fromNKJP =
    concatForestLeaves . mapForest (onEither f g)
  where
    f NKJP.NE.NE{..} = NE 
        { neType    = neType
        , subType   = subType
        , derivType = NKJP.NE.derivType <$> derived }
    g NKJP.MX.Seg{..} = if nps
        then [Nothing, Just tok]
        else [Just tok]
      where
        tok = Tok
            { orth  = orth
            , lexs  = concatMap (fromLex $ snd choice) lexs }
    fromLex ch NKJP.MX.Lex{..} =
        [ Lex { base = base              , ctag = ctag'
              , disamb = chBase == base && chCtag == ctag' }
        | (_, msd) <- msds
        , let msd' = if L.null msd then msd else L.cons ':' msd
        , let ctag' = L.append ctag msd' ]
        where (chBase, chCtag) = second L.tail $ L.break (==':') ch
