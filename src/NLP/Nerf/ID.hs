{-# LANGUAGE GeneralizedNewtypeDeriving #-}


-- | Identifier data type.


module NLP.Nerf.ID
( ID (..)
, IDMap
, idenList
, idenForest
, unidForest
) where


import qualified Data.Map.Strict as M
import qualified Data.Traversable as R

import           Data.Named.Tree


-- | Token identifier with dummy parameter representing the
-- type of the original token.
newtype ID a = ID { unID :: Int }
    deriving (Eq, Ord, Enum, Num)


-- | ID map.
type IDMap a = M.Map (ID a) a


-- | Identify list, i.e. replace leaves with identifiers.
idenList :: [a] -> ([ID a], IDMap a)
idenList xs =
    let m = M.fromList [(ID i, x) | (i, x) <- zip [0..] xs]
    in  (M.keys m, m)


-- | Identify NE forest, i.e. replace leaves with identifiers.
idenForest :: NeForest a b -> (NeForest a (ID b), IDMap b)
idenForest =
    swap . R.mapAccumL idenTree M.empty
  where
    swap (x, y) = (y, x)
    idenTree = R.mapAccumL idenNode
    idenNode m (Left x)  = (m, Left x)
    idenNode m (Right y) =
        let k = ID $ M.size m
        in  (M.insert k y m, Right k)


-- | Reverse the `idenForest` operation.
unidForest :: IDMap b -> NeForest a (ID b) -> NeForest a b
unidForest m =
    map unidTree
  where
    unidTree = fmap unidNode
    unidNode (Left x)  = Left x
    unidNode (Right y) = Right $ lkup y m
    lkup x mp  = case M.lookup x mp of
        Nothing -> error "Nerf.ID.unidForest: no key"
        Just y  -> y

