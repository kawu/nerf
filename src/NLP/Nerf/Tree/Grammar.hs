module NLP.Nerf.Tree.Grammar
( Grammar (..)
, labels
, byChildren
) where

import NLP.Nerf.Tree.Types

-- | A grammar.
data Grammar = Grammar 

-- | List grammar labels.
-- CAUTION: in the alpha1 function we assume that it is of a form
-- [0, 1, ..., lbNum].  What to do with this?
labels :: Grammar -> [Lb]
labels = undefined

-- | List all rules with the given left and right children labels,
-- respectively.
byChildren :: Grammar -> Lb -> Lb -> [Rule]
byChildren = undefined
