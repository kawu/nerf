{-# LANGUAGE RecordWildCards #-}

module NLP.Nerf.Tree.Alpha
( AC (..)
, alpha0
) where

import Control.Applicative ((<$>))
import Data.List (foldl')

import NLP.Nerf.Tree.Types
import qualified NLP.Nerf.Tree.PQueue as PQ
import qualified NLP.Nerf.Tree.PosMap as PM
import qualified NLP.Nerf.Tree.Grammar as G

-- | Alpha computation data.
data AC a = AC {
    -- | Update value in the computation node.  Different functions
    -- can be used to represent different computations.  For example,
    -- `max` will make the alpha computation behave like a max-product
    -- algorithm.  Function `(+)`, on the other hand, can be used to
    -- perform sum-product.
    --
    -- /Assumption/: `add` is a commutative function.
      add       :: a -> a -> a
    -- /Assumption/: `mul` is a commutative function.
    , mul       :: a -> a -> a
    -- | Computation strategy.
    , strat     :: Strat a
    -- | Grammar.
    , grammar   :: G.Grammar
    -- | Potential on a given position.
    , phiPos    :: Pos -> a
    -- | Rule potential.
    , phiRule   :: Rule -> a }

-- | First phase of the alpha computation.  Apart from the `AC` structure
-- only the length of the sentence has to be given.
alpha0 :: AC a -> Int -> PM.PM a
alpha0 ac@AC{..} n =
    snd $ iter (step ac) (q0, PM.empty)
  where
    q0 = foldl' update PQ.empty ws
    ws = [ (Node k k x, phiPos k)
         | x <- G.labels grammar
         , k <- Pos <$> [1..n] ]
    update q (v, x) = PQ.update add v x q

-- | Iterate function as long as the result is `Just`.
iter :: (a -> Maybe a) -> a -> a
iter f x = case f x of
    Just x' -> iter f x'
    Nothing -> x

-- | Step in the alpha0 algorithm.
step :: AC a -> (PQ.PQ a, PM.PM a) -> Maybe (PQ.PQ a, PM.PM a)
step ac@AC{..} (pq, pm) = case PQ.popMin pq of
    Nothing             -> Nothing
    Just ((v, x), pq')  -> Just
        ( if strat (beg v) (end v) x
            then joinAdj ac pm v x pq'
            else pq'
        , PM.insert v x pm )

-- | Join adjacent computation nodes and update the queue.
joinAdj :: AC a -> PM.PM a -> Node -> a -> PQ.PQ a -> PQ.PQ a
joinAdj AC{..} pm v x pq =
    foldl' update pq (leftAdj ++ rightAdj)
  where
    update q (w, y) = PQ.update add w y q
    leftAdj =
        [ join (w, y) (v, x) r
        | (w, y) <- PM.endOn (dec $ beg v) pm
        , r <- G.byChildren grammar (val w) (val v) ]
    rightAdj =
        [ join (v, x) (w, y) r
        | (w, y) <- PM.begOn (inc $ end v) pm
        , r <- G.byChildren grammar (val v) (val w) ]
    join (w, y) (w', y') r =
        ( Node (beg w) (end w') (top r)
        , mul (mul y y') (phiRule r) )
    inc = Pos . (\y -> y+1) . unPos
    dec = Pos . (\y -> y-1) . unPos
