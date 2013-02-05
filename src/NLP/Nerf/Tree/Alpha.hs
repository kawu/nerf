{-# LANGUAGE RecordWildCards #-}

module NLP.Nerf.Tree.Alpha
( TNum (..)
, AC0 (..)
, alpha0
) where

import Control.Applicative ((<$>))
import Data.List (foldl')
-- import qualified Data.Vector as V

import NLP.Nerf.Tree.Types
import qualified NLP.Nerf.Tree.PQueue as PQ
import qualified NLP.Nerf.Tree.PosMap as PM
import qualified NLP.Nerf.Tree.Grammar as G

-- | Tree ''numeric'' functions.  The structure can be used to represent
-- max-product and sum-product family of algorithms.  In particular, the
-- @a@ type can represent the most probable tree or even the set of all
-- possible trees. 
data TNum a b = TNum {
    -- | Update value in the computation node.  /Assumption/: `add` is a
    -- commutative function.
      add       :: a -> a -> a
    -- | Compute parent value given children values and `phiRule` value.
    , mul       :: a -> b -> a -> a }

-- | Alpha computation data.
data AC0 a b = AC0 {
    -- | Tree ''numeric'' functions.
      tnum      :: TNum a b
    -- | Computation strategy.
    , strat     :: Strat a
    -- | Grammar.
    , grammar   :: G.Grammar
    -- | Potential on a given position.
    , phiPos    :: Pos -> Lb -> a
    -- | Rule potential.
    , phiRule   :: Rule -> b
    -- | Transition potential.
    , phiTrans  :: Trans -> a }

-- | First phase of the alpha computation.  Apart from the `AC0` structure
-- only the length of the sentence has to be given.
alpha0 :: AC0 a b -> Int -> PM.PM a
alpha0 ac@AC0{..} n =
    snd $ iter (step ac) (q0, PM.empty)
  where
    q0 = foldl' update PQ.empty ws
    ws = [ (Node k k x, phiPos k x)
         | x <- G.labels grammar
         , k <- Pos <$> [0..n-1] ]
    update q (v, x) = PQ.update (add tnum) v x q

-- | Iterate function as long as the result is `Just`.
iter :: (a -> Maybe a) -> a -> a
iter f x = case f x of
    Just x' -> iter f x'
    Nothing -> x

-- | Step in the alpha0 algorithm.
step :: AC0 a b -> (PQ.PQ a, PM.PM a) -> Maybe (PQ.PQ a, PM.PM a)
step ac@AC0{..} (pq, pm) = case PQ.popMin pq of
    Nothing             -> Nothing
    Just ((v, x), pq')  -> Just
        ( if strat (beg v) (end v) x
            then joinAdj ac pm v x pq'
            else pq'
        , PM.insert v x pm )

-- | Join adjacent computation nodes and update the queue.
joinAdj :: AC0 a b -> PM.PM a -> Node -> a -> PQ.PQ a -> PQ.PQ a
joinAdj AC0{..} pm v x pq =
    foldl' update pq (leftAdj ++ rightAdj)
  where
    update q (w, y) = PQ.update (add tnum) w y q
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
        , mul tnum y (phiRule r) y' )
    inc = Pos . (\y -> y+1) . unPos
    dec = Pos . (\y -> y-1) . unPos

-- alpha1
--     :: AC a
--     -> Int
--     -> PM.PM a      -- ^ Results of `alpha0`
--     -> V.Vector (V.Vector a)
-- alpha1 AC{..} n pm =
--   where
--     f i j = (vect V.! i) V.! j -- ^ TODO: check vect ! i ! j, no parens
--     vect = V.fromList
--         [ V.replicate (G.labelNum grammar) zero
--             [ compute i e
--             | e <- G.labels grammar ]
--         | i <- [0..n-1] ]
--     compute i e = 
--         [ 
--         | (v, x) <- PM.endOn i pm ]
