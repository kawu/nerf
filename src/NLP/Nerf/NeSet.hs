{-# LANGUAGE ScopedTypeVariables #-}


-- | Set representation of a NE forest.


module NLP.Nerf.NeSet
( NeSet
, fromNeForest
, toNeForest
, resolveOverlap

-- * Flatten
, flattenForest
, flattenTree
) where


import qualified Data.List as L
import           Data.Ord (comparing)
import qualified Data.Set as S
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           Data.Named.Tree
import           NLP.Nerf.ID


-- | Set representation of a NE forest.
type NeSet a b = M.Map (ID b, ID b) a


-- | Transform NE forest to the set representation.
fromNeForest :: NeForest a (ID b) -> NeSet [a] b
fromNeForest =
    -- toList . fst . fromForest
    fst . fromForest
  where
    -- toList m = [NE x p q | ((p, q), x) <- M.toList m]
    fromForest xs =
        let (nss, rss) = unzip $ map fromTree xs
            ran (p, q) (p', q') = (min p p', max q q')
        in  (M.unions nss, foldl1 ran rss)
    fromTree (Node (Left x) xs) = 
        let (ns, ran@(p, q)) = fromForest xs
        in  (M.insertWith (++) (p, q) [x] ns, ran)
    fromTree (Node (Right y) _) = (M.empty, (y, y))


-- | Transfrom NE set representation to NE forest.
toNeForest :: forall a b. S.Set (ID b) -> NeSet a b -> NeForest a (ID b)
toNeForest idSet =
    consumeForest . toNeList idSet
  where
    -- | Consume NE forest from the list.
    consumeForest :: NeList a b -> NeForest a (ID b)
    consumeForest [] = []
    consumeForest xs =
        let (t, ys) = consumeTree xs
        in  t : consumeForest ys
    -- | Consume NE tree from the list.
    consumeTree :: NeList a b -> (NeTree a (ID b), NeList a b)
    consumeTree [] = error "Nerf.NeSet.toNeForest.consumeTree: empty list"
    consumeTree (ne@(Left (_, _, x)) : elems) =
        let (children, rest)  = L.span (`within` ne) elems
        in  (Node (Left x) (consumeForest children), rest)
    consumeTree (Right x : elems) =
        (Node (Right x) [], elems)


---------------------------------------------------------------
-- NeList
---------------------------------------------------------------


-- | List representation of a NE forest.
-- In comparison to NeSet, it preserves tokens as well.
-- It is sorted by spans in a way that embedded elements
-- will be positioned immediately after the mother NE element. 
type NeList a b = [NeElem a b]


-- | Element of a NE list.
type NeElem a b = Either (ID b, ID b, a) (ID b)


-- | Extract range of the given element.
range :: NeElem a b -> (ID b, ID b)
range (Left (p, q, _)) = (p, q)
range (Right p)        = (p, p)


-- | Is one element within another element?
within :: NeElem a b -> NeElem a b -> Bool
within x y =
    p >= p' && q <= q'
  where
    (p , q)  = range x
    (p', q') = range y


-- | Make an NE list from `NeSet` and the set of segment identifiers.
toNeList :: S.Set (ID b) -> NeSet a b -> NeList a b
toNeList s m =
    sort $ nes ++ ses
  where
    nes  = [Left (p, q, x) | ((p, q), x) <- M.toList m]
    ses  = map Right $ S.toList s
    sort = L.sortBy $ comparing $ \x -> case x of
        (Left (ID p, ID q, _)) -> (p, -q, False)
        (Right (ID p))         -> (p, -p, True)


---------------------------------------------------------------
-- Resolve overlapping
---------------------------------------------------------------


-- | Yet another type for representing NE sets.
type NeMap a b = Map (ID b) (Map (ID b) a)


toNeMap :: NeSet a b -> NeMap a b
toNeMap neSet = M.fromListWith M.union
    [ (p, M.singleton q x)
    | ((p, q), x) <- M.toList neSet ]


fromNeMap :: NeMap a b -> NeSet a b
fromNeMap neMap = M.fromList
    [ ((p, q), x)
    | (p, m) <- M.toList neMap
    , (q, x) <- M.toList m ]


-- | Resolve overlapping cases from the given NE set.
resolveOverlap :: forall a b. S.Set (ID b) -> NeSet a b -> NeSet a b
resolveOverlap idSet neSet =

    fromNeMap $ go $ S.toAscList idSet

  where

    -- | Alternative representation of the NE set.
    neMap0 :: NeMap a b
    neMap0 = toNeMap neSet

    -- | Recursive computation.
    go :: [ID b] -> NeMap a b
    go (k:ks) =
        M.insert k go_k go_ks
      where
        go_ks = go ks
        go_k  = M.fromList
            [ (q', x)
            | (q , x) <- begOn k neMap0
            , let q' = resolve k q go_ks ]
    go [] = M.empty

    -- | NEs beginning on the given position.
    begOn :: ID b -> NeMap a b -> [(ID b, a)]
    -- begOn k neMap = M.toList $ lkup k neMap
    begOn k neMap = case M.lookup k neMap of
        Just y  -> M.toList y
        Nothing -> []

    -- | Determine final position of the NE so that it will not
    -- overlap with any other existing (on positions starting
    -- with k+1) names.
    resolve :: ID b -> ID b -> NeMap a b -> ID b
    resolve p q neMap = max q $ maximumDef q
        [ maximumDef q $ map fst $ begOn k neMap
        | k <- [p+1 .. q] ]

--     -- | Just a safe version of the M.! function.
--     lkup x m  = case M.lookup x m of
--         Nothing -> error "Nerf.NeSet.resolveOverlap: no key"
--         Just y  -> y


---------------------------------------------------------------
-- Misc
---------------------------------------------------------------


-- | Flatten NE forest.
flattenForest :: NeForest [a] b -> NeForest a b
flattenForest = map flattenTree


-- | Flatten NE tree.
flattenTree :: NeTree [a] b -> NeTree a b
flattenTree (Node (Left [x]) cs) =
    Node (Left x) (flattenForest cs)
flattenTree (Node (Left (x:xs)) cs) =
    Node (Left x) [flattenTree $ Node (Left xs) cs]
flattenTree (Node (Right x) _) =
    Node (Right x) []
flattenTree (Node (Left []) _) =
    error "Nerf.NeSet.flattenTree: empty Left node"


-- | Maximum with a default value (for the empty-list case).
maximumDef :: Ord a => a -> [a] -> a
maximumDef x [] = x
maximumDef _ xs = maximum xs
