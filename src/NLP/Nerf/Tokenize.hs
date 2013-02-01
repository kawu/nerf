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
, moveNEs
) where

import Control.Monad ((>=>))
import Data.Foldable (foldMap)
import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.Tree as T
import qualified Data.Traversable as Tr
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified NLP.Tokenize as Tok

import Data.Named.Tree (NeForest, NeTree, groupForestLeaves)

---------------------------
-- Tokenization definition.
---------------------------

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
-- Synchronizing named entities with new sentence tokenization.
---------------------------------------------------------------

-- | A class of objects which can be converted to `String`.
class Word a where
    word :: a -> String

instance Word String where
    word = id

instance Word Text.Text where
    word = Text.unpack

instance Word LazyText.Text where
    word = LazyText.unpack

essence :: Word a => a -> Int
essence = length . filter (not . Char.isSpace) . word
{-# INLINE essence #-}

-- | Syncronization between two sentences.  Each (xs, ys) pair represents
-- tokens from the two input sentences which corresponds to each other.
type Sync a b = [([a], [b])]

-- | Synchronize two tokenizations of the sentence.
sync :: (Word a, Word b) => [a] -> [b] -> Sync a b
sync = sync' 0

sync' :: (Word a, Word b) => Int -> [a] -> [b] -> Sync a b
sync' r (x:xs) (y:ys)
    | n + r == m    = ([x], [y])    : sync' 0       xs    ys
    | n + r  < m    = join x        $ sync' (n + r) xs (y:ys)
    | otherwise     = swap . join y $ sync' (m - r) ys (x:xs)
  where
    n = essence x
    m = essence y
    join l ((ls, rs) : ps)  = (l:ls, rs) : ps
    join _ []               = error "sync'.join: bad arguments"
    swap ((ls, rs) : ps)    = (rs, ls) : swap ps
    swap []                 = []
sync' 0 [] [] = []
sync' _ _  _  = error "sync': bad arguments"

-- | Match the `Sync` with the given list, return the matching result
-- (snd elements of the `Sync` list) and the rest of the `Sync` list.
match :: (Word a, Word b) => [a] -> Sync a b -> ([b], Sync a b)
match xs ss =
    let (sl, sr) = splitAcc isMatch 0 ss
    in  (concatMap snd sl, sr)
  where
    n = sum (map essence xs)
    isMatch r (ys, _)
        | m + r < n     = (m + r, False)
        | m + r == n    = (m + r, True)
        | otherwise     = error "match.isMatch: no match"
      where
        m = sum (map essence ys)

-- | Split the list with the help of the accumulating function.
splitAcc :: (acc -> a -> (acc, Bool)) -> acc -> [a] -> ([a], [a])
splitAcc _ _ [] = ([], [])
splitAcc f acc (x:xs)
    | cond      = ([x], xs)
    | otherwise = join x (splitAcc f acc' xs)
  where
    (acc', cond) = f acc x
    join y (ys, zs) = (y:ys, zs)

-- | List forest leaves.
leaves :: NeForest a b -> [b]
leaves = concatMap $ foldMap (either (const []) (:[]))

unGroupLeaves :: NeForest a [b] -> NeForest a b
unGroupLeaves = concatMap unGroupLeavesT

unGroupLeavesT :: NeTree a [b] -> [NeTree a b]
unGroupLeavesT (T.Node (Left v) xs)     =
    [T.Node (Left v) (unGroupLeaves xs)]
unGroupLeavesT (T.Node (Right vs) _)   =
    [T.Node (Right v) [] | v <- vs]

substGroups :: (Word b, Word c) => NeForest a [b] -> Sync b c -> NeForest a [c]
substGroups fs ss = snd $ L.mapAccumL substGroupsT ss fs

substGroupsT
    :: (Word b, Word c)
    => Sync b c -> NeTree a [b]
    -> (Sync b c, NeTree a [c])
substGroupsT =
    Tr.mapAccumL f
  where
    f s (Left v)  = (s, Left v)
    f s (Right v) =
        let (v', s') = match v s
        in  (s', Right v')

-- | Synchronize named entities with tokenization represented
-- by the second function argument.  Of course, both arguments
-- should relate to the same sentence.
moveNEs :: (Word b, Word c) => NeForest a b -> [c] -> NeForest a c
moveNEs ft ys
    = unGroupLeaves
    $ substGroups
        (groupForestLeaves true ft)
        (sync (leaves ft) ys)
  where
    true _ _ = True
