module NLP.Nerf.Tree.Alpha.Node
( Node (..)
) where

import NLP.Nerf.Tree.Types

-- | A computation node.
data Node = Node
    { beg   :: {-# UNPACK #-} !Ps
    , label :: {-# UNPACK #-} !Lb
    , end   :: {-# UNPACK #-} !Ps }
    deriving (Show, Eq, Ord)
