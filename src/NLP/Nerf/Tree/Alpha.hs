{-# LANGUAGE RecordWildCards #-}

module NLP.Nerf.Tree.Alpha
( AC (..)
, TNum (..)
, SNum (..)
, alpha0
, alpha1
) where

import Control.Applicative ((<$>))
import Data.List (foldl')
import qualified Data.Vector as V

import NLP.Nerf.Tree.Types
import qualified NLP.Nerf.Tree.PQueue as PQ
import qualified NLP.Nerf.Tree.PosMap as PM
import qualified NLP.Nerf.Tree.Grammar as G

-- | Alpha computation data.
data AC a b = AC {
    -- | Computation strategy.
      strat     :: Strat a
    -- | Grammar.
    , grammar   :: G.Grammar
    -- | Potential on a given position.
    , phiPos    :: Pos -> Lb -> a
    -- | Rule potential.
    , phiRule   :: Rule -> b
    -- | Transition potential.
    , phiTrans  :: Trans -> Phi }

-- | Tree ''numeric'' functions.  The structure can be used to represent
-- max-product and sum-product family of algorithms.  In particular, the
-- @a@ type can represent the most probable tree or even the set of all
-- possible trees. 
data TNum a b = TNum {
    -- | Update value assigned to the computation node.
    -- /Assumption/: `add` is a commutative function.
      add0      :: a -> a -> a
    -- | Compute parent value given children values and `phiRule` value.
    , mul0      :: a -> b -> a -> a }

-- | Sequence computations.
data SNum a b = SNum {
      zero      :: b
    , add1      :: b -> b -> b
    , mul1      :: b -> Phi -> a -> b }

-- | First phase of the alpha computation.  Apart from the `AC0` structure
-- only the length of the sentence has to be given.
alpha0 :: AC a b -> TNum a b -> Int -> PM.PM a
alpha0 ac@AC{..} tn@TNum{..} n =
    snd $ iter (step ac tn) (q0, PM.empty)
  where
    q0 = foldl' update PQ.empty ws
    ws = [ (Node k k x, phiPos k x)
         | x <- G.labels grammar
         , k <- Pos <$> [0..n-1] ]
    update q (v, x) = PQ.update add0 v x q

-- | Iterate function as long as the result is `Just`.
iter :: (a -> Maybe a) -> a -> a
iter f x = case f x of
    Just x' -> iter f x'
    Nothing -> x

-- | Step in the alpha0 algorithm.
step :: AC a b -> TNum a b -> (PQ.PQ a, PM.PM a) -> Maybe (PQ.PQ a, PM.PM a)
step ac@AC{..} tn@TNum{..} (pq, pm) = case PQ.popMin pq of
    Nothing             -> Nothing
    Just ((v, x), pq')  -> Just
        ( if strat (beg v) (end v) x
            then joinAdj ac tn pm v x pq'
            else pq'
        , PM.insert v x pm )

-- | Join adjacent computation nodes and update the queue.
joinAdj :: AC a b -> TNum a b -> PM.PM a -> Node -> a -> PQ.PQ a -> PQ.PQ a
joinAdj AC{..} TNum{..} pm v x pq =
    foldl' update pq (leftAdj ++ rightAdj)
  where
    update q (w, y) = PQ.update add0 w y q
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
        , mul0 y (phiRule r) y' )

alpha1
    :: AC a b
    -> SNum a c
    -> Int
    -> PM.PM a
    -> V.Vector (V.Vector c)
alpha1 AC{..} SNum{..} n pm =
    vect
  where
    f i' e'
        | i < 0     = zero
        | otherwise = vect V.! i V.! e
      where
        i = unPos i'
        e = unLb  e'
    vect = V.fromList
        [ V.accum add1
             (V.replicate (G.lbNum grammar) zero)
             (no i ++ on i)
        | i <- Pos <$> [0.. n - 1] ]
    -- With no NE on the i-th position.
    no i =
        [ (unLb e, f (dec i) e)
        | e <- G.labels grammar ]
    -- With NE ending on the i-th position.
    on i =
        [ ( unLb e
          , mul1 (f (dec $ beg u) e') (phiTrans tr) x )
        | (u, x) <- PM.endOn i pm
        , let e = val u
        , e' <- G.labels grammar
        , let tr = Trans e' e ]

inc :: Pos -> Pos
inc = Pos . (\y -> y+1) . unPos
{-# INLINE inc #-}

dec :: Pos -> Pos
dec = Pos . (\y -> y-1) . unPos
{-# INLINE dec #-}
