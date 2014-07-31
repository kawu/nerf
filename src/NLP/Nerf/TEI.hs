{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Suport for tagging TEI ann_morphosyntax.xml files.


module NLP.Nerf.TEI
( nerPara
, nerSent
, toWord

-- -- * Dummy
-- , dummyNER

-- * Printing
, showTEI
) where


import           Control.Applicative
-- import           Control.Arrow (second)
import           Data.Foldable (foldMap)
import           Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           Data.String (IsString)
import qualified Control.Monad.State.Strict as S

import           Text.HTML.TagSoup (Tag (..))
import qualified Text.HTML.TagSoup as TS
import           Text.XML.PolySoup (renderTree)

-- import qualified Data.Named.Tree as N
import           Data.Named.Tree
import           Data.Tagset.Positional (Tagset, parseTag)

import qualified Text.NKJP.Morphosyntax as X
import qualified Text.NKJP.Named as N

import           NLP.Nerf.Types
import           NLP.Nerf.Utils


-----------------------------------------------------------
-- DUMMY
-----------------------------------------------------------


-- -- | Dummy NER function.
-- dummyNER :: [X.Seg t] -> NeForest NE (X.Seg t)
-- -- dummyNER = map $ flip Node [] . Right
-- dummyNER xs = (:[]) $ Node (Left "placeName")
--     [ Node (Left "innerName")
--         $ map (flip Node [] . Right) xs ]


-----------------------------------------------------------
-- NER on TEI NKJP
-----------------------------------------------------------


-- -- | A morphosyntactic segment.
-- data Word = Word {
--     -- | An orthographic form,
--       orth  :: Orth
--     -- | No preceding space.
--     , nps   :: Bool
--     -- | Morphosyntactic description.
--     , msd   :: Maybe P.Tag }
--     deriving (Show)
--
-- 
-- Seg t = Seg
--     { segID     :: t 
--     , orth      :: t
--     , nps       :: Bool
--     , lexs      :: [Lex t]
--     , choice    :: (t, t) }
--     deriving (Show, Functor)


-- | Convert a TEI segment to a Nerf `Word`.
toWord :: Tagset -> X.Seg T.Text -> Word
toWord tagset seg@X.Seg{..} = Word
    { orth  = orth
    , nps   = nps
    , msd   = Just $ parseTag tagset tag }
  where
    tag = T.intercalate ":" $ filter (not . T.null) [ctag, msdp]
    (_, ctag, msdp) = X.chosen seg


--------------------
-- NER TEI
--------------------



-- | Tag a paragraph in the TEI NKJP morphosyntax format.
nerPara
    :: ( [X.Seg L.Text]
      -> NeForest NE (X.Seg L.Text))    -- ^ NER function
    -> X.Para L.Text                    -- ^ TEI NKJP morphosyntax paragraph
    -> N.Para L.Text                    -- ^ TEI NKJP "named" paragraph
nerPara f X.Para{..} = N.Para
    { paraID = paraID   -- TODO: set new identifier to the NE paragraph. 
    , sentences = map (nerSent f) sentences }


-- | Tag a sentence in the TEI NKJP morphosyntax format.
nerSent
    :: ( [X.Seg L.Text]
       -> NeForest NE (X.Seg L.Text))   -- ^ NER function
    -> X.Sent L.Text                    -- ^ TEI NKJP morphosyntax sentence
    -> N.Sent L.Text                    -- ^ TEI NKJP "named" sentence
nerSent f X.Sent{..} = N.Sent
    { sentID = sentID   -- TODO: set new identifier
    , names = fromForest $ f segments  }


-- | Convert the forest of NEs to TEI NKJP representation. 
fromForest :: NeForest NE (X.Seg L.Text) -> [N.NE L.Text]
fromForest = internal . evalID . teiNeForest


-- | Convert NEs in a forest to the TEI NKJP representation.
teiNeForest
    :: NeForest NE (X.Seg L.Text)
    -> ID (NeForest (N.NE L.Text) (X.Seg L.Text))
teiNeForest = mapM teiNeTree


-- | Convert NEs in a tree to the TEI NKJP representation.
teiNeTree
    :: NeTree NE (X.Seg L.Text)
    -> ID (NeTree (N.NE L.Text) (X.Seg L.Text))
teiNeTree (Node n ts) = case n of
    Left x -> do
        i <- newID
        ts' <- teiNeForest ts
        let n' = defNE
                { N.neID = L.pack (show i)
                , N.orth = getOrth ts
                , N.neType = L.fromStrict
                    $ fromJust' "teiNeTree: unspecified neType"
                    $ M.lookup "type" x
                , N.subType = L.fromStrict
                    <$> M.lookup "subtype" x
                , N.derived = fmap derivType $ L.fromStrict
                    <$> M.lookup "derivtype" x
                , N.ptrs = map (ptrFrom . rootLabel) ts' }
        return $ Node (Left n') ts'
    Right x -> return $ Node (Right x) []
  where
    fromJust' e Nothing = error e
    fromJust' _ (Just x) = x
    derivType x = N.Deriv { N.derivType = x, N.derivFrom = "" }


-- | Make pointer from a node.
ptrFrom :: Either (N.NE L.Text) (X.Seg L.Text) -> N.Ptr L.Text
ptrFrom (Left x) = N.Local (N.neID x)
ptrFrom (Right x) = N.Global (X.segID x) "ann_morphosyntax.xml"


-- | Determine the `orth` value.
getOrth :: NeForest a (X.Seg L.Text) -> L.Text
getOrth =
    L.strip . L.concat . map toOrth . concatMap leaves
  where
    toOrth :: X.Seg L.Text -> L.Text
    toOrth X.Seg{..} = if nps
        then orth
        else L.cons ' ' orth


-- | Default NE.
defNE :: N.NE L.Text
defNE = N.NE
    { neID      = ""
    , derived   = Nothing
    , neType    = ""
    , subType   = Nothing
    , orth      = ""
    , base      = Nothing
    , cert      = Nothing
    , certComment = Nothing
    , ptrs      = [] }


----------------------------
-- ID Monad
----------------------------


-- | Factory of identifiers (monad).
type ID = S.State Int


-- | Evaluate the ID monad.
evalID :: ID a -> a
evalID = flip S.evalState 0


-- | Get new ID.
newID :: ID Int
newID = S.state $ \x -> (x, x+1)


----------------------------
-- NER on TEI -- utilities
----------------------------


-- | Get list of internal nodes.
internal :: NeForest a b -> [a]
internal = concatMap . foldMap $ either (:[]) (const [])


-----------------------------------------------------------
-- Printing
-----------------------------------------------------------


-- | Convert TEI (NEs) into XML representation.
showTEI :: [N.Para L.Text] -> L.Text
showTEI
    = addProlog . addEpilog . TS.renderTagsOptions opts 
    . renderTree . prettify " " "   " . teiToXML
  where
    opts = TS.renderOptions { TS.optMinimize = const True }
    addProlog = (L.append prolog)
    addEpilog = (`L.append` epilog)


-- | Prolog.
prolog :: L.Text
prolog =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<teiCorpus xmlns=\"http://www.tei-c.org/ns/1.0\" xmlns:nkjp=\"http://www.nkjp.pl/ns/1.0\" xmlns:xi=\"http://www.w3.org/2001/XInclude\">\n\
    \ <xi:include href=\"NKJP_1M_header.xml\"/>\n\
    \ <TEI>\n\
    \  <xi:include href=\"header.xml\"/>\n\
    \  <text>\n\
    \   "

    
-- | Epilog.
epilog :: L.Text
epilog =
    "\n  </text>\n\
    \ </TEI>\n\
    \</teiCorpus>"


-- | Convert TEI (NEs) to XML tree.
teiToXML :: [N.Para L.Text] -> Tree (Tag L.Text)
teiToXML xs = mkNode "body" []
    -- (sperse breakLine $ map paraToXML xs)
    (map paraToXML xs)


paraToXML :: N.Para L.Text -> Tree (Tag L.Text)
paraToXML N.Para{..} = mkNode "p" [("xml:id", paraID)]
    -- (sperse breakLine $ map sentToXML sentences)
    (map sentToXML sentences)


sentToXML :: N.Sent L.Text -> Tree (Tag L.Text)
sentToXML N.Sent{..} = mkNode "s" [("xml:id", sentID)]
    -- (sperse breakLine $ map nameToXML names)
    (map nameToXML names)


nameToXML :: N.NE L.Text -> Tree (Tag L.Text)
nameToXML ne@N.NE{..} = mkNode "seg" [("xml:id", neID)]
    -- $ sperse breakLine
    $ nameToFS ne : map ptrToXML ptrs


nameToFS :: N.NE L.Text -> Tree (Tag L.Text)
nameToFS N.NE{..} = mkNode "fs" [("type", "named")]
    -- $ sperse breakLine $ catMaybes
    $ catMaybes
    [ Just $ strToXML "orth" orth
    , Just $ symToXML "type" neType
    , symToXML "subtype" <$> subType
    , symToXML "derivtype" . N.derivType <$> derived ]


symToXML :: L.Text -> L.Text -> Tree (Tag L.Text)
symToXML attr x = mkNode "f" [("name", attr)]
    [mkNode "symbol" [("value", x)] []]


strToXML :: L.Text -> L.Text -> Tree (Tag L.Text)
strToXML attr x = mkNode "f" [("name", attr)]
    [mkNode "string" [] [mkText x]]


ptrToXML :: N.Ptr L.Text -> Tree (Tag L.Text)
ptrToXML x = mkNode "ptr" [("target", showPtr x)] []


showPtr :: N.Ptr L.Text -> L.Text
showPtr (N.Local x) = x
showPtr (N.Global x y) = L.concat [y, "#", x]


-------------------------------------------------
-- Prettify
-------------------------------------------------


-- -- | Pretify the tree.
-- prettifyTree :: L.Text -> Tree (Tag L.Text) -> Tree (Tag L.Text)
-- prettifyTree ind = preTree ind ""


-- | Add break-lines and indentation.
prettify
    :: L.Text -> L.Text
    -> Tree (Tag L.Text)
    -> Tree (Tag L.Text)
prettify ind acc t = case t of
    Node x []                       -> Node x []
    Node _ [Node (TagText _) []]    -> t
    Node x xs                       -> Node x $
        preForest ind acc xs


-- | Add break-lines and indentation.
preForest
    :: L.Text -> L.Text
    -> Forest (Tag L.Text)
    -> Forest (Tag L.Text)
preForest ind acc =
    let acc' = L.append ind acc
        brk = mkText . L.append "\n"
    in  sperse' (brk acc') (brk acc) . map (prettify ind acc')


-------------------------------------------------
-- Printing utilities
--
-- TODO: the same are in the XCES2 module.
-------------------------------------------------


mkNode :: IsString s => s -> [TS.Attribute s] -> Forest (Tag s) -> Tree (Tag s)
mkNode x = Node . TagOpen x


mkText :: s -> Tree (Tag s)
mkText x = Node (TagText x) []


-- breakLine :: IsString s => Tree (Tag s)
-- breakLine = mkText "\n"
-- 
-- 
-- | Similar to `intersperse`, but adds the item at the beginning
-- and at the end of the list as well.
-- sperse y (x:xs) = y : x : sperse y xs 
-- sperse y [] = [y]


-- | Similar to `intersperse`, but adds the item at the beginning
-- and at the end of the list as well.  The item added at the
-- end may be different than the rest.
sperse' :: a -> a -> [a] -> [a]
sperse' y z (x:xs) = y : x : sperse' y z xs
sperse' _ z [] = [z]
