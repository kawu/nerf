module NLP.Nerf.Tree.Alpha.Open
( Open
, empty
, fromList
, update
, append
, popMin
, popMax
) where

import Control.Applicative ((<$>))
import Control.Arrow (first, second)
import Data.Monoid (mappend)
import Data.List (foldl')
import qualified Data.Map as M

import NLP.Nerf.Tree.Types
import NLP.Nerf.Tree.Alpha.Node

newtype QNode = QNode { unQNode :: Node } deriving (Show, Eq)

instance Ord QNode where
    compare (QNode (Node p x q)) (QNode (Node p' x' q')) =
        compare (s p q) (s p' q')   `mappend`
        compare p p'                `mappend`
        compare x x'
      where
        s (Ps i) (Ps j) = j - i

-- | A priority queue.
newtype Open a = Open { _unOpen :: M.Map QNode a }

-- | Make priority queue with given sentence length.
empty :: Open a
empty = Open $ M.empty

-- | Make priority queue from a list.
fromList :: (a -> a -> a) -> [(Node, a)] -> Open a
fromList f xs = append f xs empty

-- | Update element in the queue (or insert new element it there
-- is no such element in the queue).
update :: (a -> a -> a) -> Node -> a -> Open a -> Open a
update f n x (Open m) = Open $ M.insertWith f (QNode n) x m

-- | Append elements to the queue.
append :: (a -> a -> a) -> [(Node, a)] -> Open a -> Open a
append f xs op =
    let upd q (v, x) = update f v x q
    in  foldl' upd op xs

-- | Pop minimal element from the queue.
popMin :: Open a -> Maybe ((Node, a), Open a)
popMin (Open m)
    =   second Open
    .   first (first unQNode)
    <$> M.minViewWithKey m

-- | Pop maximal element from the queue.
popMax :: Open a -> Maybe ((Node, a), Open a)
popMax (Open m)
    =   second Open
    .   first (first unQNode)
    <$> M.maxViewWithKey m
