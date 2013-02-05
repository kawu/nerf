module NLP.Nerf.Tree.Grammar
( Grammar (..)
, labels
, byChildren
) where

import NLP.Nerf.Tree.Types

-- | A grammar.
data Grammar = Grammar 

-- | List grammar labels.
labels :: Grammar -> [Lb]
labels = undefined

-- | List all rules with the given left and right children labels,
-- respectively.
byChildren :: Grammar -> Lb -> Lb -> [Rule]
byChildren = undefined
