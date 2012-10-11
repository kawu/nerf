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

import Control.Applicative ((<$>))
import Data.Binary (encodeFile, decodeFile)
import Data.Text.Binary ()
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M

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

-- | A NeDict is a map from forms to NE types.  Each NE may be annotated
-- with multiple types.
type NeDict = M.Map Form (S.Set NeType)

-- | Construct the dictionary from the list of entries.
mkDict :: [Entry] -> NeDict
mkDict xs = M.fromListWith S.union
    [(neOrth x, S.singleton $ neType x) | x <- xs]

-- | Remove dictionary entries which do not satisfy the predicate.
siftDict :: (Form -> S.Set NeType -> Bool) -> NeDict -> NeDict
siftDict f dict = M.fromList [(k, v) | (k, v) <- M.assocs dict, f k v]

-- | Save the dictionary in the file.
saveDict :: FilePath -> NeDict -> IO ()
saveDict = encodeFile

-- | Load the dictionary from the file.
loadDict :: FilePath -> IO NeDict
loadDict = decodeFile

-- | Merge dictionary resources.
merge :: [NeDict] -> NeDict
merge = M.unionsWith S.union

-- | Differentiate labels from separate dictionaries using
-- dictionary-unique prefixes.
diff :: [NeDict] -> [NeDict]
diff ds =
    [ mapS (addPrefix i) <$> dict
    | (i, dict) <- zip [0..] ds ]

-- | Map function over the set.
mapS :: Ord a => (a -> a) -> S.Set a -> S.Set a
mapS f s = S.fromList [f x | x <- S.toList s]
{-# INLINE mapS #-}

-- | Add integer prefix.
addPrefix :: Int -> T.Text -> T.Text
addPrefix = T.append . T.pack . show
