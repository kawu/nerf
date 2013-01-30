{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | The module implements the tokenization used within Nerf
-- and some other tokenization-related stuff.

module NLP.Nerf.Tokenize
(
-- * Tokenization
  tokenize
-- * Synchronization
, Word
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

-- | A class of objects with size.
class Word a where
    size        :: a -> Int
    rmSpaces    :: a -> a

instance Word String where
    size = length
    rmSpaces = filter (not . Char.isSpace)

instance Word Text.Text where
    size = Text.length
    rmSpaces = Text.filter (not . Char.isSpace)

instance Word LazyText.Text where
    size = fromInteger . toInteger . LazyText.length
    rmSpaces = LazyText.filter (not . Char.isSpace)

essence :: Word a => a -> Int
essence = size . rmSpaces
{-# INLINE essence #-}

-- | Syncronization between two sentences.  Each (xs, ys) pair represents
-- tokens from the two input sentences which corresponds to each other.
type Sync a = [([a], [a])]

-- | Synchronize two tokenizations of the sentence.
sync :: Word a => [a] -> [a] -> Sync a
sync = sync' 0

sync' :: Word a => Int -> [a] -> [a] -> Sync a
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
match :: Word a => [a] -> Sync a -> ([a], Sync a)
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

substGroups :: Word b => NeForest a [b] -> Sync b -> NeForest a [b]
substGroups fs ss = snd $ L.mapAccumL substGroupsT ss fs

substGroupsT :: Word b => Sync b -> NeTree a [b] -> (Sync b, NeTree a [b])
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
moveNEs :: Word b => NeForest a b -> [b] -> NeForest a b
moveNEs ft ys
    = unGroupLeaves
    $ substGroups
        (groupForestLeaves true ft)
        (sync (leaves ft) ys)
  where
    true _ _ = True
