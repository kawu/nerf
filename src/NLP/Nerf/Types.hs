{-# LANGUAGE RecordWildCards #-}


-- | Basic Nerf types.


module NLP.Nerf.Types
( Orth
, Word (..)
, NE
, Ob
, Lb
) where


import           Control.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Text as T
-- import qualified Data.Binary as B
import           Data.Binary (Binary, put, get)

import qualified Data.Tagset.Positional as P
import qualified Data.Named.IOB as IOB


-- | An orthographic form.
type Orth = T.Text


-- | A morphosyntactic segment.
data Word = Word {
    -- | An orthographic form,
      orth  :: Orth
    -- | No preceding space.
    , nps   :: Bool
    -- | Morphosyntactic description.
    , msd   :: Maybe P.Tag }
    deriving (Show)

instance Binary Word where
    put Word{..} = put orth >> put nps >> put msd
    get = Word <$> get <*> get <*> get


-- | A named entity.  It has a complex structure for the sake of flexibility.
-- In particular, the following type can be used both to represent simplex
-- labels as well as NE lables consisting of several components (main type,
-- subtype, derivation type and so on, as in TEI NKJP).
type NE = M.Map T.Text T.Text


-- | An observation consist of an index (of list type) and an actual
-- observation value.
type Ob = ([Int], T.Text)


-- | A label is created by encoding the named entity forest using the
-- IOB method.
type Lb = IOB.Label NE
