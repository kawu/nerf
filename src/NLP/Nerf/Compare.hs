-- | Compare two NE-annotated datasets.


module NLP.Nerf.Compare
( Stats (..)
, (.+.)
, compare
) where


import           Prelude hiding (span, compare)
import           Control.Applicative ((<$>))
import           Control.Monad (forM)
import qualified Control.Monad.State.Strict as ST
import qualified Control.Monad.Writer.Strict as W
import qualified Data.Traversable as Tr
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.Text as T

import qualified Data.Named.Tree as N


-- | Statistics.
data Stats = Stats
    { fp    :: !Int     -- ^ false positive
    , tp    :: !Int     -- ^ true positive
    , fn    :: !Int     -- ^ false negative
    , tn    :: !Int     -- ^ true negative
    } deriving (Show, Eq, Ord)


-- | A NE represented by its label and a character-level span, over which
-- the NE is stretched.  White-space characters do not count when computing
-- the span.
data Node a = Node
    { label :: a
    , _span :: (Int, Int)
    } deriving (Show, Eq, Ord)


-- | A union of two spans.
spanUnion :: (Int, Int) -> (Int, Int) -> (Int, Int)
spanUnion (p0, q0) (p1, q1) = (min p0 p1, max q0 q1)


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
    :: Ord a
    => [ ( N.NeForest a T.Text
         , N.NeForest a T.Text) ]
    -> M.Map a Stats
compare xs = M.unionsWith (.+.)
    [ cmpNodes (nodesF $ toIDs x) (nodesF $ toIDs y)
    | (x, y) <- xs ]


-- | Compare two sets of `Node`s.  The function is label-sensitive.
cmpNodes :: Ord a => S.Set (Node a) -> S.Set (Node a) -> M.Map a Stats
cmpNodes x y = M.fromList
    [ (key, mkStats (with key x) (with key y))
    | key <- S.toList keys ]
  where
    keys    = S.union (getKeys x) (getKeys y)
    getKeys = S.fromList . map label . S.toList
    with k  = S.filter ((==k).label)


-- | Compare two sets of `Node`s.  The function is label-insensitive.
mkStats :: Ord a => S.Set (Node a) -> S.Set (Node a) -> Stats
mkStats x y = Stats
    { fp    = S.size (S.difference y x)
    , tp    = S.size (S.intersection x y)
    , fn    = S.size (S.difference x y)
    , tn    = 0 }


-- | Replace words with character-level position identifiers.
-- White-spaces are ignored.
toIDs :: N.NeForest a T.Text -> N.NeForest a (Int, Int)
toIDs ts = flip ST.evalState 0 $ forM ts $ Tr.mapM $ \e -> case e of
    Left x  -> return (Left x)
    Right x -> do
        let k = T.length $ T.filter (not.C.isSpace) x
        i <- ST.get
        ST.put $ i + k
        return $ Right (i, i + k)


-- | Extract the set of nodes from the NE forest.
nodesF :: Ord a => N.NeForest a (Int, Int) -> S.Set (Node a)
nodesF = S.unions . map nodesT


-- | Extract the set of nodes from the NE tree.
nodesT :: Ord a => N.NeTree a (Int, Int) -> S.Set (Node a)
nodesT = W.execWriter . mkNode


-- | Make `Node` from a tree.  Return the span of the tree.
mkNode
    :: Ord a => N.NeTree a (Int, Int)
    -> W.Writer (S.Set (Node a)) (Int, Int)
mkNode (N.Node (Right i) _) = return i
mkNode (N.Node (Left neType) xs) = do
    span <- foldl1 spanUnion <$> mapM mkNode xs
    W.tell $ S.singleton $ Node neType span
    return span
