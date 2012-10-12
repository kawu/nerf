-- | Basic types.

module NLP.Nerf.Types
( Word
, NE
, Ob
, Lb
) where

import qualified Data.Text as T
import qualified Data.Named.IOB as IOB

-- | A word.
type Word = T.Text

-- | A named entity.
type NE = T.Text

-- | An observation consist of an index (of list type) and an actual
-- observation value.
type Ob = ([Int], T.Text)

-- | A label is created by encoding the named entity forest using the
-- IOB method.
type Lb = IOB.Label NE
