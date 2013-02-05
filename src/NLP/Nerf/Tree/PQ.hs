module NLP.Nerf.Tree.PQ
( PQ
, mkPQ
, update
, popMin
, popMax
) where

import Control.Applicative ((<$>))
import Control.Arrow (first, second)
import Data.Monoid (mappend)
import qualified Data.Map as M

import NLP.Nerf.Tree.Types

newtype QNode = QNode { unQNode :: Node } deriving (Show, Eq)

instance Ord QNode where
    compare (QNode (Node p q x)) (QNode (Node p' q' x')) =
        compare (s p q) (s p' q')   `mappend`
        compare p p'                `mappend`
        compare x x'
      where
        s (Pos i) (Pos j) = j - i

-- | A priority queue.
newtype PQ a = PQ { _unPQ :: M.Map QNode a }

-- | Make priority queue with given sentence length.
mkPQ :: PQ a
mkPQ = PQ $ M.empty

-- | Update element in the queue (or insert new element it there
-- is no such element in the queue).
update :: (a -> a -> a) -> Node -> a -> PQ a -> PQ a
update f n x (PQ m) = PQ $ M.insertWith f (QNode n) x m

-- | Pop minimal element from the queue.
popMin :: PQ a -> Maybe ((Node, a), PQ a)
popMin (PQ m)
    =   second PQ
    .   first (first unQNode)
    <$> M.minViewWithKey m

-- | Pop maximal element from the queue.
popMax :: PQ a -> Maybe ((Node, a), PQ a)
popMax (PQ m)
    =   second PQ
    .   first (first unQNode)
    <$> M.maxViewWithKey m
