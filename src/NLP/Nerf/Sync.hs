-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE OverloadedStrings #-}


module NLP.Nerf.Sync
( sync
, xcesLen
) where


import qualified Data.Char as Char
import qualified Data.Text as T

import qualified NLP.Nerf.XCES2 as XCES
import qualified NLP.Nerf.Tokenize as Tok
import           NLP.Nerf.Utils


---------------------------------------------------------------
-- Synchronization
---------------------------------------------------------------


-- | Syncronize the two lists given size functions of their
-- individual elements.
sync
    :: (a -> Int)       -- ^ Size of `a`
    -> (b -> Int)       -- ^ Size of `b`
    -> [a] -> [b]
    -> [([a], [b])]
sync f g =
    go 0 0 ([], [])
  where
    go i j acc (x:xs) (y:ys)
        | i' > j'   = go i  j' (addSnd y acc) (x:xs) ys
        | i' < j'   = go i' j  (addFst x acc) xs  (y:ys)
        | otherwise =
            let z = revBoth $ addFst x $ addSnd y acc
            in  z : go 0 0 ([], []) xs ys
      where
        i' = i + f x
        j' = j + g y
    go _ _ _ [] [] = []
    go _ _ _ [] _  = error "Nerf.Sync: empty xs"
    go _ _ _ _  [] = error "Nerf.Sync: empty ys"
    addFst x (xs, ys) = (x:xs, ys)
    addSnd y (xs, ys) = (xs, y:ys)
    revBoth (xs, ys)  = (reverse xs, reverse ys)


-- | Number of non-space characters in the sentence.
xcesLen :: XCES.Sent -> Int
xcesLen = 
    sum . map (weight . orth) . concatMap leaves
  where
    weight = length . filter (not . Char.isSpace)
    orth (Just x) = T.unpack $ XCES.orth x
    orth Nothing  = ""
