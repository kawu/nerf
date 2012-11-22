{-# LANGUAGE TupleSections #-}

-- | Basic types for dictionary handling. 

module NLP.Nerf.Dict.Base
(
-- * Lexicon entry
  NeType
, Form
, isMultiWord
, Entry (..)

-- * Dictionary
, Dict
, fromPairs
, fromEntries
, siftDict
, saveDict
, loadDict

-- * Merging dictionaries
, merge
, diff
) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, encodeFile, decodeFile)
import Data.Text.Binary ()
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.DAWG as D

-- | A type of named entity.
type NeType = T.Text

-- | A orthographic form.
type Form = T.Text

-- | Is the form a multiword one?
isMultiWord :: Form -> Bool
isMultiWord = (>1) . length . T.words

-- | A Named Entity entry from the LMF dictionary.
data Entry = Entry
    { neOrth :: !Form	-- ^ Orthographic form of the NE
    , neType :: !NeType -- ^ Type of the NE
    } deriving (Show, Read, Eq, Ord)

-- | A 'Dict' is a map from forms to labels.  Each form may be annotated
-- with multiple labels.  The map is represented using the directed acyclic
-- word graph.
type Dict a = D.DAWG (S.Set a)

-- | Construct dictionary from the list of form/neType pairs.
fromPairs :: Ord a => [(Form, a)] -> Dict a
fromPairs xs = D.fromListWith S.union
    [ ( T.unpack x
      , S.singleton y)
    | (x, y) <- xs ]

-- | Construct dictionary from the list of entries.
fromEntries :: [Entry] -> Dict NeType
fromEntries = fromPairs . map ((,) <$> neOrth <*> neType)

-- | Remove dictionary entries which do not satisfy the predicate.
siftDict :: Ord a => (Form -> S.Set a -> Bool) -> Dict a -> Dict a
siftDict f dict = D.fromList [(k, v) | (k, v) <- D.assocs dict, f (T.pack k) v]

-- | Save the dictionary in the file.
saveDict :: (Ord a, Binary a) => FilePath -> Dict a -> IO ()
saveDict = encodeFile

-- | Load the dictionary from the file.
loadDict :: (Ord a, Binary a) => FilePath -> IO (Dict a) 
loadDict = decodeFile

-- | Merge dictionary resources.
merge :: Ord a => [Dict a] -> Dict a
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
diff :: Ord a => [Dict a] -> [Dict (a, Int)]
diff ds =
    [ mapS (,i) `mapD` dict
    | (i, dict) <- zip [0..] ds ]

-- | Map function over the DAWG elements.
mapD :: Ord b => (a -> b) -> D.DAWG a -> D.DAWG b
mapD f d = D.fromList [(x, f y) | (x, y) <- D.assocs d]

-- | Map function over the set.
mapS :: Ord b => (a -> b) -> S.Set a -> S.Set b
mapS f s = S.fromList [f x | x <- S.toList s]
