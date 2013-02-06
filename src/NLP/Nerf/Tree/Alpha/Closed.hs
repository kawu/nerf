{-# LANGUAGE RecordWildCards #-}

module NLP.Nerf.Tree.Alpha.Closed
( Closed
, empty
, insert
, begOn
, endOn
) where

import qualified Data.IntMap as M

import NLP.Nerf.Tree.Types
import NLP.Nerf.Tree.Alpha.Node

-- | A position map.
data Closed a = Closed {
    -- | Set of nodes beginning on a given position.
      _begOn :: M.IntMap [(Node, a)]
    -- | Set of nodes ending on a given position.
    , _endOn :: M.IntMap [(Node, a)] }

instance Functor Closed where
    fmap f (Closed b e) =
        let g xs = [(n, f x) | (n, x) <- xs]
        in  Closed (fmap g b) (fmap g e)

-- | Empty position map.
empty :: Closed a
empty = Closed M.empty M.empty

-- | Insert element to a position map.  TODO: some optimization?
-- The current definition can easily lead to a memory leak or a
-- stack overflow.
insert :: Node -> a -> Closed a -> Closed a
insert n x Closed{..} = Closed
    (M.insertWith (++) (unPs $ beg n) [(n, x)] _begOn)
    (M.insertWith (++) (unPs $ end n) [(n, x)] _endOn)

-- | Return all elements which begin on a given position.
begOn :: Ps -> Closed a -> [(Node, a)]
begOn i Closed{..} = M.findWithDefault [] (unPs i) _begOn

-- | Return all elements which end on a given position.
endOn :: Ps -> Closed a -> [(Node, a)]
endOn i Closed{..} = M.findWithDefault [] (unPs i) _endOn
