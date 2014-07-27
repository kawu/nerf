{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


-- | The module implements the tokenization used within Nerf
-- and some other tokenization-related stuff.


module NLP.Nerf.Tokenize
(
-- * Tokenization
  tokenize
-- * Synchronization
, Word (..)
, sync
) where


import           Control.Arrow (second)
import           Control.Monad ((>=>))
import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.Tree as T
import qualified Data.Traversable as Tr
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified NLP.Tokenize as Tok
import qualified Data.IntervalMap.Strict as I

import           Data.Named.Tree (NeForest, NeTree)

import qualified NLP.Nerf.Types as Nerf


-------------------------------------
-- Tokenization definition
-------------------------------------


-- | Default tokenizator.
defaultTokenizer :: Tok.Tokenizer
defaultTokenizer
    =   Tok.whitespace
    >=> Tok.uris
    >=> Tok.punctuation


-- | Tokenize sentence using the default tokenizer.
tokenize :: String -> [String]
tokenize = Tok.run defaultTokenizer


---------------------------------------------------------------
-- BACKUP
---------------------------------------------------------------


-------------------------------------
-- Word
-------------------------------------


-- | A class of objects which can be converted to `String`.
class Word a where
    word :: a -> String


instance Word String where
    word = id


instance Word Text.Text where
    word = Text.unpack


instance Word LazyText.Text where
    word = LazyText.unpack


instance Word Nerf.Word where
    word = word . Nerf.orth


essence :: Word a => a -> Int
essence = length . filter (not . Char.isSpace) . word
{-# INLINE essence #-}


-------------------------------------
-- Grouping leaves
-------------------------------------


unGroupLeaves :: NeForest a [b] -> NeForest a b
unGroupLeaves = concatMap unGroupLeavesT


unGroupLeavesT :: NeTree a [b] -> [NeTree a b]
unGroupLeavesT (T.Node (Left v) xs)     =
    [T.Node (Left v) (unGroupLeaves xs)]
unGroupLeavesT (T.Node (Right vs) _)   =
    [T.Node (Right v) [] | v <- vs]


---------------------------------------------------------------
-- Identifying ranges
---------------------------------------------------------------


type Range = I.Interval Int


-- | Range computation step.
ranged :: Word a => Int -> a -> (Int, (Range, a))
ranged p w =
    (q, (i, w))
  where
    q = p + essence w
    i = I.IntervalCO p q


-- | Compute ranges of individual tokens.
rangedList :: Word a => [a] -> [(Range, a)]
rangedList = snd . L.mapAccumL ranged 0


-- | Compute ranges of individual tokens.
rangedForest :: Word b => NeForest a b -> NeForest a (Range, b)
rangedForest = 
    snd . L.mapAccumL (Tr.mapAccumL f) 0
  where
    f acc (Left x)  = (acc, Left x)
    f acc (Right x) =
        let (acc', y) = ranged acc x
        in  (acc', Right y)
        

---------------------------------------------------------------
-- Synchronizing named entities with new sentence tokenization
---------------------------------------------------------------


-- | Replace leaves in the NE forest with corresponding tokens.
replaceToks
    :: I.IntervalMap Int c
    -> NeForest a (Range, b)
    -> ( I.IntervalMap Int c
       , NeForest a (Range, c) )
replaceToks ivMap nes
    = second unGroupLeaves
    $ L.mapAccumL (Tr.mapAccumL replace) ivMap nes
  where
    replace im (Left x) = (im, Left x)
    replace im (Right (ran, _)) =
        let rsXs = I.intersecting im ran
            im'  = L.foldl' (flip I.delete) im (map fst rsXs)
        in  (im', Right rsXs)


-- | Lift the first range of a tree to the top.
liftRange :: NeTree a (Range, b) -> (Range, NeTree a b)
liftRange (T.Node (Left v) xs) =
    (ran, T.Node (Left v) (map snd ys))
  where
    ys = map liftRange xs
    ran = maybeHead $ map fst ys
    maybeHead (x:_) = x
    maybeHead []    = error "liftRange: invalid NE tree"
liftRange (T.Node (Right (ran, v)) _) = (ran, T.Node (Right v) [])


-- | Synchronize the list of NE trees with the new tokenization.
sync
    :: (Word b, Word c)
    => NeForest a b     -- ^ NE forest
    -> [c]              -- ^ New tokenization
    -> NeForest a c     -- ^ Resulting NE forest
sync nes0 xs0
    = map snd . I.toList . I.fromList
    $ map (second mkLeaf) (I.toList ivMap')
    ++ map liftRange nes'
  where
    -- Interval map of the new tokenization
    ivMap = I.fromList $ rangedList xs0
    -- NE non-leaf trees with ranges
    nes = filter internal $ rangedForest nes0
    -- Replace tokens...
    (ivMap', nes') = replaceToks ivMap nes
    -- Is it an internal node?
    internal x = case T.rootLabel x of
        Left _  -> True
        Right _ -> False
    -- Make a leaf tree
    mkLeaf x = T.Node (Right x) []
