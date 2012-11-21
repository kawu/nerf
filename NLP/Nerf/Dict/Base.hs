-- | Basic types for dictionary handling. 

module NLP.Nerf.Dict.Base
(
-- * Lexicon entry
  Form
, isMultiWord
, NeType
, Entry (..)

-- * Dictionary
, NeDict
, mkDict
, siftDict
, saveDict
, loadDict

-- * Merging dictionaries
, merge
, diff
) where

import Data.Binary (encodeFile, decodeFile)
import Data.Text.Binary ()
import qualified Data.Text as T
import qualified Data.Set as S
-- import qualified Data.Map as M
import qualified Data.DAWG as D

-- | A orthographic form.
type Form = T.Text

-- | Is the form a multiword one?
isMultiWord :: Form -> Bool
isMultiWord = (>1) . length . T.words

-- | A type of named entity.
type NeType = T.Text

-- | A Named Entity entry from the LMF dictionary.
data Entry = Entry
    { neOrth :: !Form	-- ^ Orthographic form of the NE
    , neType :: !NeType -- ^ Type of the NE
    } deriving (Show, Read, Eq, Ord)

-- | A NeDict is a map from forms to NE types.  Each form may be annotated
-- with multiple types.  The map is represented using the directed acyclic
-- word graph.
type NeDict = D.DAWG (S.Set NeType)

-- | Construct the dictionary from the list of entries.
mkDict :: [Entry] -> NeDict
mkDict xs = D.fromListWith S.union
    [ ( T.unpack (neOrth x)
      , S.singleton (neType x))
    | x <- xs]

-- | Remove dictionary entries which do not satisfy the predicate.
siftDict :: (Form -> S.Set NeType -> Bool) -> NeDict -> NeDict
siftDict f dict = D.fromList [(k, v) | (k, v) <- D.assocs dict, f (T.pack k) v]

-- | Save the dictionary in the file.
saveDict :: FilePath -> NeDict -> IO ()
saveDict = encodeFile

-- | Load the dictionary from the file.
loadDict :: FilePath -> IO NeDict
loadDict = decodeFile

-- | Merge dictionary resources.
merge :: [NeDict] -> NeDict
merge = unionsWith S.union

-- | Replacement implementation of unionsWith while there is
-- no such function in dawg library. 
unionsWith :: Ord a => (a -> a -> a) -> [D.DAWG a] -> D.DAWG a
unionsWith f = foldl (unionWith f) D.empty

-- | Replacement implementation of unionWith while there is
-- no such function in dawg library. 
unionWith :: Ord a => (a -> a -> a) -> D.DAWG a -> D.DAWG a -> D.DAWG a
unionWith f d d' = D.fromListWith f (D.assocs d ++ D.assocs d')

-- | Differentiate labels from separate dictionaries using
-- dictionary-unique prefixes.
diff :: [NeDict] -> [NeDict]
diff ds =
    [ mapS (addPrefix i) `mapD` dict
    | (i, dict) <- zip [0..] ds ]

-- | Map function over the DAWG elements.
mapD :: Ord b => (a -> b) -> D.DAWG a -> D.DAWG b
mapD f d = D.fromList [(x, f y) | (x, y) <- D.assocs d]

-- | Map function over the set.
mapS :: Ord b => (a -> b) -> S.Set a -> S.Set b
mapS f s = S.fromList [f x | x <- S.toList s]

-- | Add integer prefix.
addPrefix :: Int -> T.Text -> T.Text
addPrefix = T.append . T.pack . show
