{-# LANGUAGE RecordWildCards #-}


-- | Utility functions.


module NLP.Nerf.Utils
( leaves
) where


import qualified Data.Foldable as F
import           Data.Named.Tree


-- | Extract leaves of a tree.
leaves :: NeTree a b -> [b]
leaves = F.foldMap $ either (const []) (:[])
