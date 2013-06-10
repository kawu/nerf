-- | Compare two NE-annotated datasets.


module NLP.Nerf.Compare
( Stats (..)
, compare
) where


import           Prelude hiding (span, compare)
import           Control.Applicative ((<$>))
import           Control.Monad (forM)
import qualified Control.Monad.State.Strict as ST
import qualified Control.Monad.Writer.Strict as W
import qualified Data.Traversable as T
import qualified Data.Set as S
import qualified Data.Map as M

import qualified Data.Named.Tree as N


-- | Statistics.
data Stats = Stats
    { fp    :: !Int     -- ^ false positive
    , tp    :: !Int     -- ^ true positive
    , fn    :: !Int     -- ^ false negative
    , tn    :: !Int     -- ^ true negative
    } deriving (Show, Eq, Ord)


-- | A NE represented by its label and a set of corresponding words.
data Node a b = Node
    { label :: a
    , _span :: S.Set b }
    deriving (Show, Eq, Ord)


-- | Add stats.
(.+.) :: Stats -> Stats -> Stats
x .+. y = Stats
    { fp    = fp x + fp y
    , tp    = tp x + tp y
    , fn    = fn x + fn y
    , tn    = tn x + tn y }


-- | Compare two NE-annotated datasets.  The function assumes, that
-- forest pairs correspond to the same sentences.
compare
    :: (Ord a, Ord b)
    => [(N.NeForest a b, N.NeForest a b)]
    -> M.Map a Stats
compare xs = M.unionsWith (.+.)
    [ cmpNodes (nodesF $ toIDs x) (nodesF $ toIDs y)
    | (x, y) <- xs ]


-- | Compare two sets of `Node`s.  The function is label-sensitive.
cmpNodes
    :: (Ord a, Ord b)
    => S.Set (Node a b)
    -> S.Set (Node a b)
    -> M.Map a Stats
cmpNodes x y = M.fromList
    [ (key, mkStats (with key x) (with key y))
    | key <- S.toList keys ]
  where
    keys    = S.union (getKeys x) (getKeys y)
    getKeys = S.fromList . map label . S.toList
    with k  = S.filter ((==k).label)


-- | Compare two sets of `Node`s.  The function is label-insensitive.
mkStats
    :: (Ord a, Ord b)
    => S.Set (Node a b)
    -> S.Set (Node a b)
    -> Stats
mkStats x y = Stats
    { fp    = S.size (S.difference y x)
    , tp    = S.size (S.intersection x y)
    , fn    = S.size (S.difference x y)
    , tn    = 0 }


-- | Replace words with position identifiers.
toIDs :: N.NeForest a b -> N.NeForest a Int
toIDs ts = flip ST.evalState 0 $ forM ts $ T.mapM $ \e -> case e of
    Left x  -> return (Left x)
    Right _ -> do
        i <- ST.get
        ST.put $ i + 1
        return (Right i)


-- | Extract the set of nodes from the NE forest.  
nodesF :: (Ord a, Ord b) => N.NeForest a b -> S.Set (Node a b)
nodesF = S.unions . map nodesT


-- | Extract the set of nodes from the NE tree.  
nodesT :: (Ord a, Ord b) => N.NeTree a b -> S.Set (Node a b)
nodesT = W.execWriter . mkNode


-- | Make `Node` from a tree.  Return the span of the tree.
mkNode :: (Ord a, Ord b) => N.NeTree a b
       -> W.Writer (S.Set (Node a b)) (S.Set b)
mkNode (N.Node (Left neType) xs) = do
    span <- S.unions <$> mapM mkNode xs
    W.tell $ S.singleton $ Node neType span
    return span
mkNode (N.Node (Right i) _) = do
    return $ S.singleton i
