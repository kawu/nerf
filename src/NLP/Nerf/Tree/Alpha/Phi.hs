module NLP.Nerf.Tree.Alpha.Phi
( Phi (..)
, Strat (..)
, Ext (..)
, Comp (..)
) where

import NLP.Nerf.Tree.Types

-- | A stucture which specifies how to assign @a@ values to individual
-- computation nodes. 
data Comp a = Comp {
    -- | Value for a given label and a position.
      compLeaf  :: Lb -> Ps -> a
    -- | Value for a given rule and children values.
    , compRule  :: a -> Rule -> a -> a
    -- | Determine value from values acquired by different `compRule` calls
    -- (or different `compLeaf` calls)?
    , compJoin  :: a -> a -> a }

-- | Extended computation with additional tracking of @b@ values
-- dependent on @a@ values.
data Ext a b = Ext {
      extLeaf   :: Lb -> Ps -> a -> b
    , extRule   :: (a, b) -> Rule -> (a, b) -> a -> b
    , extJoin   :: (a, b) -> (a, b) -> a -> b }

-- | A computation strategy consists of an extended computation and
-- a cutting function which specifies when to stop computations.
data Strat a b = Strat {
      strExt    :: Ext a b
    , strCut    :: (a, b) -> Bool }

-- | Finally, a phi computation.
data Phi a b = Phi {
      comp      :: Comp a
    , active    :: a -> Bool
    , essence   :: a -> b }
