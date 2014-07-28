-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module NLP.Nerf.Sync
(
-- * Generic
  sync

-- * XCES
, syncSent
, sentLen
, wordLen
) where


-- import qualified Control.Monad.State.Strict as S
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import           Data.Named.Tree

import qualified NLP.Nerf.XCES2 as XCES
-- import qualified NLP.Nerf.Types as Types
-- import qualified NLP.Nerf.Tokenize as Tok
import           NLP.Nerf.ID
import           NLP.Nerf.NeSet
import           NLP.Nerf.Utils


---------------------------------------------------------------
-- IDs
---------------------------------------------------------------


-- | Sync two lists with identifiers.
syncIDs
    :: (a -> Int)   -- ^ Size of `a`
    -> IDMap a      -- ^ ID map for `a`s
    -> (b -> Int)   -- ^ Size of `b`
    -> IDMap b      -- ^ ID map for `b`s
    -> [ID a]       -- ^ First list
    -> [ID b]       -- ^ Second list
    -> Sync (ID a) (ID b)
syncIDs f fm g gm = sync (f . lookfm) (g . lookgm)
  where
    lookfm x = case M.lookup x fm of
        Nothing -> error "Nerf.Sync.syncIDs: no fm key"
        Just y  -> y
    lookgm x = case M.lookup x gm of
        Nothing -> error "Nerf.Sync.syncIDs: no gm key"
        Just y  -> y


-- | ID transformation map, that is a transormation from
-- `a` identifierst to ranges of `b` identifiest.
type TMap a b = M.Map (ID a) (ID b, ID b)


-- | Determine the ID transormation map.
mkTMap :: Sync (ID a) (ID b) -> TMap a b
mkTMap sc = M.fromList
    [ (x, (p, q))
    | (xs, ys) <- sc
    , let p = safeMin ys
    , let q = safeMax ys
    , x <- xs ]
  where
    safeMin [] = error "Nerf.Sync.mkTMap.safeMin: empty list"
    safeMin xs = minimum xs
    safeMax [] = error "Nerf.Sync.mkTMap.safeMax: empty list"
    safeMax xs = maximum xs


-- | Replace identifiers.
replIDs
    :: TMap b c
    -> NeSet a b
    -> NeSet a c
replIDs tMap neSet = M.fromList 
    [ ((p', q'), xs)
    | ((p, q), xs) <- M.toList neSet
    , let p' = fst $ lkup p tMap
    , let q' = snd $ lkup q tMap ]
  where
    lkup x m  = case M.lookup x m of
        Nothing -> error "Nerf.Sync.replIDs: no key"
        Just y  -> y


---------------------------------------------------------------
-- Summing it up
---------------------------------------------------------------


-- | Synchronize the NE forest with the new tokenization.
-- It is assumed, that both arguments represent the same sentence.
syncNeForest
    :: forall a b c
    .  (b -> Int)
    -> (c -> Int)
    -> NeForest a b
    -> [c]
    -> NeForest a c
syncNeForest seg1Len seg2Len sent tok2 =

    unidForest tok2IDMap
        $ flattenForest
        $ toNeForest (M.keysSet tok2IDMap) neSet

  where

    -- Replace segments with identifiers
    (sentID, tok1IDMap) = idenForest sent   :: (NeForest a (ID b), IDMap b)
    (tok2ID, tok2IDMap) = idenList tok2     :: ([ID c], IDMap c)
    tok1ID = concatMap leaves sentID        :: [ID b]
    
    -- Synchronize the two tokenizations
    syncMap = mkTMap ( syncIDs
        seg1Len tok1IDMap
        seg2Len tok2IDMap
        tok1ID tok2ID )                     :: TMap b c

    -- NE set representation with new identifiers
    neSet = resolveOverlap (M.keysSet tok2IDMap)
          $ replIDs syncMap
          $ fromNeForest sentID             :: NeSet [a] c


---------------------------------------------------------------
-- XCES
---------------------------------------------------------------


-- TODO: This module should not be probably dependent on the `XCES` module.  


-- | XCES segment.
type XSeg = Maybe XCES.Tok


-- | Synchronize the XCES sentence with the new tokenization.
-- It is assumed, that both arguments represent the same sentence.
syncSent :: XCES.Sent -> [XSeg] -> XCES.Sent
syncSent = syncNeForest segmLen segmLen


-- | Number of non-space characters in an XCES sentence.
sentLen :: XCES.Sent -> Int
sentLen = sum . map segmLen . concatMap leaves


-- | Number of non-space characters in an XCES segment.
segmLen :: XSeg -> Int
segmLen = maybe 0 wordLen


-- | Number of non-space characters in an XCES word.
wordLen :: XCES.Tok -> Int
wordLen = length . filter (not . Char.isSpace) . T.unpack . XCES.orth


---------------------------------------------------------------
-- Synchronization (generic)
---------------------------------------------------------------


-- | Syncronization between two lists. Each (xs, ys) pair represents
-- elements from the two input lists corresponding to each other
-- (both xs and ys are always non-empty).
type Sync a b = [([a], [b])]


-- -- | Synchronize the two lists given size functions of their
-- -- individual elements.
-- sync
--     :: (a -> Int)
--     -> (b -> Int)
--     -> [a] -> [b]
--     -> Sync a b
-- sync f g =
--     go 0
--   where
--     go r (x:xs) (y:ys)
--         | n + r == m = ([x], [y]) : go 0 xs ys
--         | n + r < m =        join x $ go (n + r) xs (y:ys)
--         | otherwise = swap . join y $ go (m - r) ys (x:xs)
--       where
--         n = f x
--         m = g y
--         join l ((ls, rs) : ps) = (l:ls, rs) : ps
--         join _ [] = error "Nerf.Sync.sync.go.join: bad arguments"
--         swap ((ls, rs) : ps) = (rs, ls) : swap ps
--         swap [] = []
--     go 0 [] [] = []
--     go _ _ _ = error "Nerf.Sync.sync.go: bad arguments"


-- | Synchronize the two lists given size functions of their
-- individual elements.
sync
    :: (a -> Int)       -- ^ Size of `a`
    -> (b -> Int)       -- ^ Size of `b`
    -> [a] -> [b]
    -> Sync a b
sync f g =
    go 0 0 ([], [])
  where
    go i j acc (x:xs) (y:ys)
        | i' > j'   = go i  j' (addSnd y acc) (x:xs) ys
        | i' < j'   = go i' j  (addFst x acc) xs  (y:ys)
        | otherwise =
            let z = revBoth $ addFst x $ addSnd y acc
            in  z : go 0 0 ([], []) xs ys
      where
        i' = i + f x
        j' = j + g y
    go _ _ _ [] [] = []
    go _ _ _ [] _  = error "Nerf.Sync: empty xs"
    go _ _ _ _  [] = error "Nerf.Sync: empty ys"
    addFst x (xs, ys) = (x:xs, ys)
    addSnd y (xs, ys) = (xs, y:ys)
    revBoth (xs, ys)  = (reverse xs, reverse ys)


-- -- | Match the `Sync` with the given list, return the matching result
-- -- (snd elements of the `Sync` list) and the rest of the `Sync` list.
-- match :: (a -> Int) -> [a] -> Sync a b -> ([b], Sync a b)
-- match f xs ss =
--     let (sl, sr) = splitAcc isMatch 0 ss
--     in (concatMap snd sl, sr)
--   where
--     n = sum (map f xs)
--     isMatch r (ys, _)
--         | m + r < n = (m + r, False)
--         | m + r == n = (m + r, True)
--         | otherwise = error "match.isMatch: no match"
--       where
--         m = sum (map f ys)


-- ----------------------------
-- -- ID Monad
-- ----------------------------
-- 
-- 
-- -- | Factory of identifiers (monad).
-- type IDM = S.State Int
-- 
-- 
-- -- | Evaluate the ID monad.
-- evalID :: IDM a -> a
-- evalID = flip S.evalState 0
-- 
-- 
-- -- | Get new ID.
-- newID :: IDM Int
-- newID = S.state $ \x -> (x, x+1)


---------------------------------------------------------------
-- Utils
---------------------------------------------------------------


-- -- | Split the list with the help of the accumulating function.
-- splitAcc :: (acc -> a -> (acc, Bool)) -> acc -> [a] -> ([a], [a])
-- splitAcc _ _ [] = ([], [])
-- splitAcc f acc (x:xs)
--     | cond = ([x], xs)
--     | otherwise = join x (splitAcc f acc' xs)
--   where
--     (acc', cond) = f acc x
--     join y (ys, zs) = (y:ys, zs)
