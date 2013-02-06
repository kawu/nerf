{-# LANGUAGE RecordWildCards #-}

-- | The alpha computation specialized to perform the sum-product algorithm.

module NLP.Nerf.Tree.Alpha
( AC (..)
, alpha0
, alpha1
) where

import Control.Applicative ((<$>))
import qualified Data.Vector as V

import NLP.Nerf.Tree.Types
import qualified NLP.Nerf.Tree.PQueue as PQ
import qualified NLP.Nerf.Tree.PosMap as PM
import qualified NLP.Nerf.Tree.Grammar as G

-- | Alpha computation data.
data AC = AC {
    -- | Computation strategy.
      active    :: Strat
    -- | Grammar.
    , grammar   :: G.Grammar
    -- | Potential on a given position.
    , phiPos    :: Pos -> Lb -> Phi
    -- | Rule potential.
    , phiRule   :: Rule -> Phi
    -- | Transition potential.
    , phiTrans  :: Trans -> Phi }

-- | First phase of the alpha computation.  Apart from the `AC` structure
-- only the length of the sentence has to be given.
alpha0 :: AC -> Int -> PM.PM Phi
alpha0 ac@AC{..} n =
    snd $ iter (step ac) (q0, PM.empty)
  where
    q0 = foldl' update PQ.empty ws
    ws = [ (Node k k x, phiPos k x)
         | x <- G.labels grammar
         , k <- Pos <$> [0..n-1] ]
    update q (v, x) = PQ.update (+) v x q

-- | Iterate function as long as the result is `Just`.
iter :: (a -> Maybe a) -> a -> a
iter f x = case f x of
    Just x' -> iter f x'
    Nothing -> x

-- | Step in the alpha0 algorithm.
step :: AC -> (PQ.PQ Phi, PM.PM Phi) -> Maybe (PQ.PQ Phi, PM.PM Phi)
step ac@AC{..} (pq, pm) = case PQ.popMin pq of
    Nothing             -> Nothing
    Just ((v, x), pq')  -> Just
        ( if active v
            then joinAdj ac pm v x pq'
            else pq'
        , PM.insert v x pm )

-- | Join adjacent computation nodes and update the queue.
joinAdj :: AC -> PM.PM Phi -> Node -> Phi -> PQ.PQ Phi -> PQ.PQ Phi
joinAdj AC{..} pm v x pq =
    foldl' update pq (leftAdj ++ rightAdj)
  where
    update q (w, y) = PQ.update (+) w y q
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
        , y * phiRule r * y' )

alpha1
    :: AC
    -> Int
    -> PM.PM Phi
    -> V.Vector (V.Vector Phi)
alpha1 AC{..} n pm =
    vect
  where
    f i' e'
        | i < 0     = 0
        | otherwise = vect V.! i V.! e
      where
        i = unPos i'
        e = unLb  e'
    vect = V.fromList
        [ V.accum (+)
             (V.replicate (G.lbNum grammar) 0)
             (no i ++ on i)
        | i <- Pos <$> [0.. n - 1] ]
    -- With no NE on the i-th position.
    no i =
        [ (unLb e, f (dec i) e)
        | e <- G.labels grammar ]
    -- With NE ending on the i-th position.
    on i =
        [ ( unLb e
          , f (dec $ beg u) e' * phiTrans tr * x )
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
