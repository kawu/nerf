{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


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
, Tok (..)
, Lex (..)

-- * Parsing
, parseXCES

-- * Printing
, showXCES

-- * NKJP
, fromNKJP

-- * NER
, NER
, nerXCES

-- * Conversion
, fromXCES
, toXCES
) where


import           Control.Applicative
-- import           Control.Monad (void)
import           Control.Arrow (second)
import qualified Control.Monad.State.Strict as St
import           Data.String (IsString)
import           Data.Maybe (catMaybes, mapMaybe)
-- import           Data.List (intersperse)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Foldable as F
import qualified Data.Traversable as R
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

-- import           Data.Tree
-- import qualified Data.Named.Tree as Tr
-- import           Data.Named.Tree (NeForest)
import           Data.Named.Tree

import           Data.Tagset.Positional (Tagset, parseTag)

-- import           Text.StringLike (StringLike)
import           Text.HTML.TagSoup (Tag (..))
import qualified Text.HTML.TagSoup as TS

import           Text.XML.PolySoup hiding (P, Q)
import qualified Text.XML.PolySoup as P

-- For the sake of `fromNKJP` function.
import qualified Text.NKJP.Named as NKJP.NE
import qualified Text.NKJP.Morphosyntax as NKJP.MX

import           NLP.Nerf.Types (NE, Word(..))


---------------------------------------------------------------------
-- XCES data types
---------------------------------------------------------------------


-- | XCES file consists of a list of paragraphs.
type XCES = [Para]


-- | Each paragraph consists of a list of sentences (there is no
-- paragraph identifier).
type Para = [Sent]


-- | A sentence is a forest of `NE`s (kept in internal nodes)
-- with tokens (or no-space markers) stored in leaves.
-- No-space markers are represented with `Nothing` values.
--
-- `Sent` is equal to `NeForest NE (Maybe Tok)` as well.
type Sent = [Root]


-- | A root of the NE forest.
type Root = NeTree NE (Maybe Tok)


-- | A token.
data Tok = Tok
    { orth  :: T.Text
    , lexs  :: [Lex] }
    deriving (Show)


-- | A lexical entry -- a potential interpretation of a segment.
-- TODO: Change text to strict.
data Lex = Lex
    { base      :: T.Text
    , ctag      :: T.Text
    -- | Was it chosen while disambiguating?
    , disamb    :: Bool }
    deriving (Show)



---------------------------------------------------------------------
-- NER
---------------------------------------------------------------------


-- | Type of the NER function which has to be supplied to `nerXCES`.
type NER = [(Tok, Word)] -> NeForest NE (Tok, Word)


-- | NER XCES sentence.  Input NEs will be ignored.
nerXCES :: Tagset -> NER -> Sent -> Sent
nerXCES tagset ner =
    let leaves = concatMap $ F.foldMap $ either (const []) (:[])
    in  toXCES . ner . leaves . fromXCES tagset


---------------------------------------------------------------------
-- Conversion
---------------------------------------------------------------------


-- | Convert the XCES representation of a sentence into the internal
-- representation used within Nerf.
-- `Tok`ens are presereved in leaves so that it is easy to recover
-- the original XCES version.
fromXCES :: Tagset -> NeForest a (Maybe Tok) -> NeForest a (Tok, Word)
fromXCES tagset = rmNoF . addWords tagset


-- | Conversion back to XCES.
toXCES :: NeForest a (Tok, Word) -> NeForest a (Maybe Tok)
toXCES =
    concatForestLeaves . mapForest (onLeaf f)
  where
    f (t, w) = if nps w
        then [Nothing, Just t]
        else [Just t]


rmNoF :: NeForest a (Maybe b) -> NeForest a b
rmNoF = concatMap rmNoT


rmNoT :: NeTree a (Maybe b) -> NeForest a b
rmNoT (Node (Left x) xs)        = [Node (Left x) (rmNoF xs)]
rmNoT (Node (Right (Just x)) _) = [Node (Right x) []]
rmNoT (Node (Right Nothing) _)  = []


addWords :: Tagset -> NeForest a (Maybe Tok) -> NeForest a (Maybe (Tok, Word))
addWords tagset =
    evalConv . mapM (R.mapM f)
  where
    f nd = case nd of
        Left x          -> return $ Left x
        Right Nothing   -> do
            setNps True
            return $ Right Nothing
        Right (Just t)  -> do
            _nps <- getNps
            let Tok{..} = t
                -- TODO: we take only the first disam MSD into account.
                -- We could use the `Set` data structure instead.
                w = Word {orth = orth, nps = _nps, msd = mkMSD lexs}
            setNps False
            return $ Right $ Just (t, w)
    mkMSD = safeHead . mapMaybe fromLex
    fromLex Lex{..} = if disamb
        then Just $ parseTag tagset ctag
        else Nothing
    safeHead (x:_) = Just x
    safeHead [] = Nothing


-- | A conversion monad.
type Conv = St.State Bool


-- | Evaluate the `Conv` monad.
evalConv :: Conv a -> a
evalConv = flip St.evalState False


-- | Get the current `nps` state.
getNps :: Conv Bool
getNps = St.get


-- | Set the current `nps` state.
setNps :: Bool -> Conv ()
setNps = St.put


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
parseXCES :: L.Text -> XCES
parseXCES = F.concat . evalP xcesP . parseForest . TS.parseTags


-- | Parsing predicates.
type P a = P.P (XmlTree L.Text) a
type Q a = P.Q (XmlTree L.Text) a


xcesP :: P XCES
xcesP = concat <$> every' (true //> paraQ)


paraQ :: Q Para
paraQ = named "chunk" *> hasAttrVal "type" "p" /> sentQ


sentQ :: Q Sent
sentQ = named "chunk" *> hasAttrVal "type" "s" /> nodeQ


nodeQ :: Q Root
nodeQ = (flip Node [] . Right <$> segQ) <|> neTreeQ


segQ :: Q (Maybe Tok)
segQ =  (Just <$> tokQ)
    <|> (Nothing <$ nsQ)
    where nsQ = node $ named "ns"


tokQ :: Q Tok
tokQ = named "token" `joinR` (Tok
    <$> first (getTxt "orth")
    <*> every' lexQ )


lexQ :: Q Lex
lexQ = (getAttr "disamb" <$> named "lex") `join` \dmb -> ( Lex
    <$> first (getTxt "base")
    <*> first (getTxt "ctag")
    <*> pure (dmb == Just "1") )


neTreeQ :: Q Root
neTreeQ =
    mkNode <$> (neQ </> nodeQ)
  where
    mkNode (ne, xs) = Node (Left ne) xs
    neQ = M.fromList . map (both L.toStrict)
        . F.concat . getAtts <$> named neS
    both f (x, y) = (f x, f y)
--     neQ = named neS *> ( NE
--         <$> attr neTypeS
--         <*> optional (attr neSubTypeS)
--         <*> optional (attr neDerivTypeS) )


-------------------------------------------------
-- Parsing utilities
-------------------------------------------------


-- | Get contexnts of the embedded text XML node.
getTxt :: L.Text -> Q T.Text
getTxt x = named x `joinR` first (L.toStrict <$> node text)


-------------------------------------------------
-- Printing
-------------------------------------------------


-- TODO: add a more generic solution for the
-- `sperse breakLine` trick.


-- | Convert XCES into its XML representation. 
showXCES :: XCES -> L.Text
showXCES =
    let opts = TS.renderOptions { TS.optMinimize = const True }
    in  TS.renderTagsOptions opts . renderTree . xcesToXML


-- | Convert XCES to XML tree.
xcesToXML :: XCES -> Tree (Tag L.Text)
xcesToXML xs = Node
    (TagOpen "chunkList" [])
    (sperse breakLine $ map paraToXML xs)


paraToXML :: Para -> Tree (Tag L.Text)
paraToXML xs = Node
    (TagOpen "chunk" [("type", "p")])
    (sperse breakLine $ map sentToXML xs)


sentToXML :: Sent -> Tree (Tag L.Text)
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


rootToXML :: Root -> Tree (Tag L.Text)
rootToXML (Node r xs) = case r of
    Left ne -> Node neTag
        (sperse breakLine $ map rootToXML xs)
      where
        neTag = TagOpen neS . map (both L.fromStrict) $ M.toList ne
        both f (x, y) = (f x, f y)
--             [ (neTypeS, neType) ] ++
--             mbTag neSubTypeS subType ++
--             mbTag neDerivTypeS derivType
--         mbTag x y = case y of
--             Nothing -> []
--             Just z  -> [(x, z)]
    Right Nothing -> Node (TagOpen "ns" []) []
    Right (Just tok) -> tokToXML tok
    

tokToXML :: Tok -> Tree (Tag L.Text)
tokToXML Tok{..} = mkNode "token" []
    $ sperse breakLine
    $ mkNode "orth" [] [mkText orth]
    : map lexToXML lexs


lexToXML :: Lex -> Tree (Tag L.Text)
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


mkNode
    :: T.Text -> [TS.Attribute T.Text]
    -> Forest (Tag L.Text)
    -> Tree (Tag L.Text)
mkNode x = Node . fmap L.fromStrict . TagOpen x


mkText :: T.Text -> Tree (Tag L.Text)
mkText x = Node (TagText $ L.fromStrict x) []


breakLine :: Tree (Tag L.Text)
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
    -> NeForest NE (Maybe Tok)
fromNKJP =
    concatForestLeaves . mapForest (onEither f g)
  where
    f NKJP.NE.NE{..} = M.fromList $ catMaybes
        [ Just (neTypeS, L.toStrict neType)
        , (neSubTypeS,) . L.toStrict <$> subType
        , (neDerivTypeS,) . L.toStrict . NKJP.NE.derivType <$> derived ]
    g NKJP.MX.Seg{..} = if nps
        then [Nothing, Just tok]
        else [Just tok]
      where
        tok = Tok
            { orth  = L.toStrict orth
            , lexs  = concatMap (fromLex $ snd choice) lexs }
    fromLex ch NKJP.MX.Lex{..} =
        [ Lex { base = L.toStrict base, ctag = L.toStrict ctag'
              , disamb = chBase == base && chCtag == ctag' }
        | (_, msd) <- msds
        , let msd' = if L.null msd then msd else L.cons ':' msd
        , let ctag' = L.append ctag msd' ]
        where (chBase, chCtag) = second L.tail $ L.break (==':') ch
