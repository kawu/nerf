{-# LANGUAGE RecordWildCards #-}

module NLP.Nerf.Tree.Alpha.Generic
( alpha0
-- , alpha1
) where

import Control.Applicative ((<$>))
import Data.List (foldl')
import qualified Data.Vector as V

import NLP.Nerf.Tree.Types
import NLP.Nerf.Tree.Alpha.Node
import qualified NLP.Nerf.Tree.Grammar as G
import qualified NLP.Nerf.Tree.Alpha.Closed as C
import qualified NLP.Nerf.Tree.Alpha.Open as O
import qualified NLP.Nerf.Tree.Alpha.Phi as P

-- | First phase of the alpha computation.  Apart from the `AC0` structure
-- only the length of the sentence has to be given.
alpha0 :: G.Grammar -> P.Phi a b -> Int -> C.Closed a
alpha0 grammar phi@P.Phi{..} n =
    snd $ iter (step grammar phi) (q0, C.empty)
  where
    P.Comp{..} = comp
    q0 = O.fromList compJoin ws
    ws = [ (Node k x k, compLeaf x k)
         | x <- G.labels grammar
         , k <- Ps <$> [0..n-1] ]

-- | Iterate function as long as the result is `Just`.
iter :: (a -> Maybe a) -> a -> a
iter f x = case f x of
    Just x' -> iter f x'
    Nothing -> x

-- | Step in the alpha0 algorithm.
step
    :: G.Grammar -> P.Phi a b
    -> (O.Open a, C.Closed a)
    -> Maybe (O.Open a, C.Closed a)
step gr phi (pq, pm) = case O.popMin pq of
    Nothing             -> Nothing
    Just ((v, x), pq')  -> Just
        ( if P.active phi x
            then joinAdj gr phi pm v x pq'
            else pq'
        , C.insert v x pm )

-- | Join adjacent computation nodes and update the queue.
joinAdj
    :: G.Grammar -> P.Phi a b -> C.Closed a
    -> Node -> a -> O.Open a -> O.Open a
joinAdj grammar P.Phi{..} pm v x pq =
    foldl' update pq (leftAdj ++ rightAdj)
  where
    P.Comp{..} = comp
    update q (w, y) = O.update compJoin w y q
    leftAdj =
        [ join (w, y) (v, x) r
        | (w, y) <- C.endOn (dec $ beg v) pm
        , r <- G.byChildren grammar (label w) (label v) ]
    rightAdj =
        [ join (v, x) (w, y) r
        | (w, y) <- C.begOn (inc $ end v) pm
        , r <- G.byChildren grammar (label v) (label w) ]
    join (w, y) (w', y') r =
        ( Node (beg w) (top r) (end w')
        , compRule y r y' )

-- alpha1
--     :: AC a b
--     -> SNum a c
--     -> Int
--     -> PM.PM a
--     -> V.Vector (V.Vector c)
-- alpha1 AC{..} SNum{..} n pm =
--     vect
--   where
--     f i' e'
--         | i < 0     = zero
--         | otherwise = vect V.! i V.! e
--       where
--         i = unPos i'
--         e = unLb  e'
--     vect = V.fromList
--         [ V.accum add1
--              (V.replicate (G.lbNum grammar) zero)
--              (no i ++ on i)
--         | i <- Pos <$> [0.. n - 1] ]
--     -- With no NE on the i-th position.
--     no i =
--         [ (unLb e, f (dec i) e)
--         | e <- G.labels grammar ]
--     -- With NE ending on the i-th position.
--     on i =
--         [ ( unLb e
--           , mul1 (f (dec $ beg u) e') (phiTrans tr) x )
--         | (u, x) <- PM.endOn i pm
--         , let e = label u
--         , e' <- G.labels grammar
--         , let tr = Trans e' e ]

inc :: Ps -> Ps
inc = Ps . (\y -> y+1) . unPs
{-# INLINE inc #-}

dec :: Ps -> Ps
dec = Ps . (\y -> y-1) . unPs
{-# INLINE dec #-}
