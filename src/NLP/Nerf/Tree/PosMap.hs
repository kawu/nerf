{-# LANGUAGE RecordWildCards #-}

module NLP.Nerf.Tree.PosMap
( PM
, empty
, insert
, begOn
, endOn
) where

import qualified Data.Map as M

import NLP.Nerf.Tree.Types

-- | A position map.
data PM a = PM {
    -- | Set of nodes beginning on a given position.
      _begOn :: M.Map Pos [(Node, a)]
    -- | Set of nodes ending on a given position.
    , _endOn :: M.Map Pos [(Node, a)] }

-- | Empty position map.
empty :: PM a
empty = PM M.empty M.empty

-- | Insert element to a position map.  TODO: some optimization?
-- The current definition can easily lead to a memory leak or a
-- stack overflow.
insert :: Node -> a -> PM a -> PM a
insert n x PM{..} = PM
    (M.insertWith (++) (beg n) [(n, x)] _begOn)
    (M.insertWith (++) (end n) [(n, x)] _endOn)

-- | Return all elements which begin on a given position.
begOn :: Pos -> PM a -> [(Node, a)]
begOn i PM{..} = M.findWithDefault [] i _begOn

-- | Return all elements which end on a given position.
endOn :: Pos -> PM a -> [(Node, a)]
endOn i PM{..} = M.findWithDefault [] i _endOn
