{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NLP.Nerf.Tree.Types
( Pos (..)
, Ob (..)
, Lb (..)
, Phi
, Sent
, Rule (..)
, Trans (..)
, Strat
, Node (..)
-- , NCM (..)
) where

import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
import Data.Binary
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- These are internal types.  You should implement also external
-- types which could be used on the level of external tree model
-- interface.

-- | A position in a sentence (0, 1, ...).
newtype Pos = Pos { unPos :: Int } deriving
    ( Show, Read, Eq, Ord, Enum, Binary
    , Vector U.Vector, MVector U.MVector, U.Unbox )

-- | An observation (0, 1, ...).
newtype Ob = Ob { unOb :: Int } deriving
    ( Show, Read, Eq, Ord, Enum, Binary
    , Vector U.Vector, MVector U.MVector, U.Unbox )

-- | A label (0, 1, ...).
newtype Lb = Lb { unLb :: Int } deriving
    ( Show, Read, Eq, Ord, Enum, Binary
    , Vector U.Vector, MVector U.MVector, U.Unbox )

-- | A potential.
type Phi = Double

-- | An input sequence (or a sentence).
type Sent = V.Vector (U.Vector Ob)

-- | A context free grammar rule.
data Rule = Rule
    { top   :: {-# UNPACK #-} !Lb
    , left  :: {-# UNPACK #-} !Lb
    , right :: {-# UNPACK #-} !Lb }
    deriving (Show, Eq, Ord)

-- | A transition between root labels.
data Trans = Trans
    { from  :: {-# UNPACK #-} !Lb
    , to    :: {-# UNPACK #-} !Lb }
    deriving (Show, Eq, Ord)

-- | A computation node.
data Node = Node
    { beg   :: {-# UNPACK #-} !Pos
    , end   :: {-# UNPACK #-} !Pos
    , val   :: {-# UNPACK #-} !Lb }
    deriving (Show, Eq, Ord)

-- | A computation strategy.
type Strat = Node -> Bool

