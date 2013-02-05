{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NLP.Nerf.Tree.Types
( Pos (..)
, Ob (..)
, Lb (..)
, Phi
, Sent
, Rule (..)
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

-- | A position in a sentence.
newtype Pos = Pos { unPos :: Int } deriving
    ( Show, Read, Eq, Ord, Enum, Binary
    , Vector U.Vector, MVector U.MVector, U.Unbox )

-- | An observation.
newtype Ob = Ob { unOb :: Int } deriving
    ( Show, Read, Eq, Ord, Enum, Binary
    , Vector U.Vector, MVector U.MVector, U.Unbox )

-- | A label.
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

-- | A computation strategy.
type Strat = Pos -> Pos -> Lb -> Bool

-- -- | A grammar.
-- type Grammar = S.Set 

-- | A computation node.
data Node = Node
    { beg   :: {-# UNPACK #-} !Pos
    , end   :: {-# UNPACK #-} !Pos
    , val   :: {-# UNPACK #-} !Lb }
    deriving (Show, Eq, Ord)

-- -- | A Nerf computation monad.
-- data NCM a = NCM a
