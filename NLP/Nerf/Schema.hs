{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Observation schema blocks for Nerf.

module NLP.Nerf.Schema
( 
-- * Schema
  Ox
, Schema
, void
, sequenceS_

-- * Using the schema
, schematize

-- * Building schema

-- ** From config
, SchemaCfg (..)
, defaultCfg
, fromCfg

-- ** Schema blocks
, orthS
, lemmaS
, shapeS
, suffixS
, searchS
, shift
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, join)
import Data.Maybe (maybeToList)
import Data.Binary (Binary, put, get, decodeFile)
import qualified Data.Char as C
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T

import qualified Data.CRF.Chain1 as CRF
import qualified Control.Monad.Ox as Ox
import qualified Control.Monad.Ox.Text as Ox

import NLP.Nerf.Types
import qualified NLP.Nerf.Dict as Dict

-- | The Ox monad specialized to word token type and text observations.
type Ox a = Ox.Ox Word T.Text a

-- | A schema is a block of the Ox computation performed within the
-- context of the sentence and sentence position.
type Schema a = V.Vector Word -> Int -> Ox a

-- | A dummy schema block.
void :: a -> Schema a
void x _ _ = return x

-- | Sequence the list of schemas.
sequenceS_ :: [Schema a] -> Schema ()
sequenceS_ xs sent =
    let ys = map ($sent) xs
    in  \k -> sequence_ (map ($k) ys)

-- | Record structure of the basic observation types.
data BaseOb = BaseOb
    { orth          :: Int -> Maybe T.Text
    , lowOrth       :: Int -> Maybe T.Text }

-- | Construct the 'BaseOb' structure given the sentence.
mkBaseOb :: V.Vector Word -> BaseOb
mkBaseOb sent = BaseOb
    { orth      = _orth
    , lowOrth   = _lowOrth }
  where
    at          = Ox.atWith sent
    _orth       = (id `at`)
    _lowOrth i  = T.toLower <$> _orth i

-- | Orthographic observations.
orthS :: (Int -> [Int]) -> Schema ()
orthS ixs sent = \k -> do
    mapM_ (Ox.save . lowOrth)    (ixs k)
    mapM_ (Ox.save . upOnlyOrth) (ixs k)
  where
    BaseOb{..}      = mkBaseOb sent
    upOnlyOrth i    = orth i >>= \x -> case T.any C.isUpper x of
        True    -> Just x
        False   -> Nothing

-- | Lemma substitute.
lemmaS :: (Int -> [Int]) -> Schema ()
lemmaS ixs sent = \k -> do
    mapM_ lowLemma (ixs k)
  where
    BaseOb{..}      = mkBaseOb sent
    lowPrefix i j   = Ox.prefix j =<< lowOrth i
    lowSuffix i j   = Ox.suffix j =<< lowOrth i
    lowLemma i = Ox.group $ do
        mapM_ (Ox.save . lowPrefix i) [0, -1, -2, -3]
        mapM_ (Ox.save . lowSuffix i) [0, -1, -2, -3]

-- | Shape and packed shape.
shapeS :: (Int -> [Int]) -> Schema ()
shapeS ixs sent = \k -> do
    mapM_ (Ox.save . shape)  (ixs k)
    mapM_ (Ox.save . shapeP) (ixs k)
    Ox.save $ link <$> shape  k <*> shape  (k - 1)
    Ox.save $ link <$> shapeP k <*> shapeP (k - 1)
  where
    BaseOb{..}      = mkBaseOb sent
    shape i         = Ox.shape <$> orth i
    shapeP i        = Ox.pack <$> shape i
    link x y        = T.concat [x, "-", y]

-- | Several suffixes.
suffixS :: (Int -> [Int]) -> Schema ()
suffixS ixs sent = \k -> do
    forM_ (ixs k) $ \i ->
        mapM_ (Ox.save . lowSuffix i) [2, 3, 4]
  where
    BaseOb{..}      = mkBaseOb sent
    lowSuffix i j   = Ox.suffix j =<< lowOrth i

-- | Plain dictionary search. 
searchS :: Dict.NeDict -> (Int -> [Int]) -> Schema ()
searchS dict ixs sent = \k -> do
    mapM_ (Ox.saves . searchDict) (ixs k)
  where
    BaseOb{..}      = mkBaseOb sent
    searchDict i    = join . maybeToList $
        S.toList <$> (orth i >>= flip M.lookup dict)

-- | Configuration of the Ox schema for the Nerf.
data SchemaCfg = SchemaCfg
    { orthC     :: [Int] 
    , lemmaC    :: [Int]
    , shapeC    :: [Int]
    , suffixC   :: [Int]
    , dictC     :: Maybe (Dict.NeDict, [Int]) }

instance Binary SchemaCfg where
    put SchemaCfg{..} = do
        put orthC
        put lemmaC
        put shapeC
        put suffixC
        put dictC
    get = SchemaCfg
        <$> get
        <*> get
        <*> get
        <*> get
        <*> get

-- | Default configuration for Nerf observation schema.
defaultCfg
    :: FilePath     -- ^ Path to 'Dict.NeDict' in a binary form
    -> IO SchemaCfg
defaultCfg nePath = do
    neDict <- decodeFile nePath
    return $ SchemaCfg
        { orthC     = [-1, 0]
        , lemmaC    = [-1, 0]
        , shapeC    = [-1, 0]
        , suffixC   = [0]
        , dictC     = Just (neDict, [-1, 0]) }

-- | Shift elements from the input list.
shift :: [Int] -> Int -> [Int]
shift xs k = [x + k | x <- xs]

mkBasicS :: ((Int -> [Int]) -> Schema ()) -> [Int] -> Schema ()
mkBasicS _ []   = void ()
mkBasicS blk xs = blk (shift xs)

mkDictS :: Maybe (Dict.NeDict, [Int]) -> Schema ()
mkDictS (Just (d, xs))  = searchS d (shift xs)
mkDictS Nothing         = void ()

fromCfg :: SchemaCfg -> Schema ()
fromCfg SchemaCfg{..} = sequenceS_
    [ mkBasicS orthS orthC
    , mkBasicS lemmaS lemmaC
    , mkBasicS shapeS shapeC
    , mkBasicS suffixS suffixC
    , mkDictS dictC ]

-- | Use the schema to extract observations from the sentence.
schematize :: Schema a -> [Word] -> CRF.Sent Ob
schematize schema xs =
    map (S.fromList . Ox.execOx . schema v) [0 .. n - 1]
  where
    v = V.fromList xs
    n = V.length v
