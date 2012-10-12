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
, Block
, fromBlock
, orthS
, lemmaS
, shapeS
, shapePairS
, suffixS
, searchS
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
-- context of the sentence and the absolute sentence position.
type Schema a = V.Vector Word -> Int -> Ox a

-- | A dummy schema block.
void :: a -> Schema a
void x _ _ = return x

-- | Sequence the list of schemas and discard individual values.
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

-- | A block is a chunk of the Ox computation performed within the
-- context of the sentence and the list of absolute sentence positions.
type Block a = V.Vector Word -> [Int] -> Ox a

-- | Transform the block to the schema dependent on the list of
-- relative sentence positions.
fromBlock :: Block a -> [Int] -> Schema a
fromBlock blk xs sent =
    let blkSent = blk sent
    in  \k -> blkSent [x + k | x <- xs]

-- | Orthographic observations determined with respect to the
-- list of relative positions.
orthS :: Block ()
orthS sent = \ks -> do
    mapM_ (Ox.save . lowOrth)    ks
    mapM_ (Ox.save . upOnlyOrth) ks
  where
    BaseOb{..}      = mkBaseOb sent
    upOnlyOrth i    = orth i >>= \x -> case T.any C.isUpper x of
        True    -> Just x
        False   -> Nothing

-- | Lemma substitute determined with respect to the list of
-- relative positions.
lemmaS :: Block ()
lemmaS sent = \ks -> do
    mapM_ lowLemma ks
  where
    BaseOb{..}      = mkBaseOb sent
    lowPrefix i j   = Ox.prefix j =<< lowOrth i
    lowSuffix i j   = Ox.suffix j =<< lowOrth i
    lowLemma i = Ox.group $ do
        mapM_ (Ox.save . lowPrefix i) [0, -1, -2, -3]
        mapM_ (Ox.save . lowSuffix i) [0, -1, -2, -3]

-- | Shape and packed shape determined with respect to the list of
-- relative positions.
shapeS :: Block ()
shapeS sent = \ks -> do
    mapM_ (Ox.save . shape)  ks
    mapM_ (Ox.save . shapeP) ks
  where
    BaseOb{..}      = mkBaseOb sent
    shape i         = Ox.shape <$> orth i
    shapeP i        = Ox.pack <$> shape i

-- | Shape pairs determined with respect to the list of relative positions.
shapePairS :: Block ()
shapePairS sent = \ks ->
    forM_ ks $ \i -> do
        Ox.save $ link <$> shape  i <*> shape  (i - 1)
        Ox.save $ link <$> shapeP i <*> shapeP (i - 1)
  where
    BaseOb{..}      = mkBaseOb sent
    shape i         = Ox.shape <$> orth i
    shapeP i        = Ox.pack <$> shape i
    link x y        = T.concat [x, "-", y]

-- | Several suffixes determined with respect to the list of
-- relative positions.
suffixS :: Block ()
suffixS sent = \ks ->
    forM_ ks $ \i ->
        mapM_ (Ox.save . lowSuffix i) [2, 3, 4]
  where
    BaseOb{..}      = mkBaseOb sent
    lowSuffix i j   = Ox.suffix j =<< lowOrth i

-- | Plain dictionary search determined with respect to the list of
-- relative positions.
searchS :: Dict.NeDict -> Block ()
searchS dict sent = \ks -> do
    mapM_ (Ox.saves . searchDict) ks
  where
    BaseOb{..}      = mkBaseOb sent
    searchDict i    = join . maybeToList $
        S.toList <$> (orth i >>= flip M.lookup dict)

-- | Configuration of the schema.  All configuration elements specify the
-- range over which a particular observation type should be taken on account.
-- For example, the @[-1, 0, 2]@ range means that observations of particular
-- type will be extracted with respect to previous (@k - 1@), current (@k@)
-- and after the next (@k + 2@) positions when identifying the observation
-- set for position @k@ in the input sentence.
data SchemaCfg = SchemaCfg
    { orthC         :: [Int]    -- ^ The 'orthS' schema block
    , lemmaC        :: [Int]    -- ^ The 'lemmaS' schema block
    , shapeC        :: [Int]    -- ^ The 'shapeS' schema block
    , shapePairC    :: [Int]    -- ^ The 'shapePairS' schema block
    , suffixC       :: [Int]    -- ^ The 'suffixS' schema block
    , dictC     :: Maybe (Dict.NeDict, [Int]) -- ^ The 'searchS' schema block
    }

instance Binary SchemaCfg where
    put SchemaCfg{..} = do
        put orthC
        put lemmaC
        put shapeC
        put shapePairC
        put suffixC
        put dictC
    get = SchemaCfg
        <$> get
        <*> get
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
        { orthC         = [-1, 0]
        , lemmaC        = [-1, 0]
        , shapeC        = [-1, 0]
        , shapePairC    = [0]
        , suffixC       = [0]
        , dictC         = Just (neDict, [-1, 0]) }

mkBasicS :: Block () -> [Int] -> Schema ()
mkBasicS _   [] = void ()
mkBasicS blk xs = fromBlock blk xs

mkDictS :: Maybe (Dict.NeDict, [Int]) -> Schema ()
mkDictS (Just (d, xs))  = fromBlock (searchS d) xs
mkDictS Nothing         = void ()

-- | Build the schema based on the configuration.
fromCfg :: SchemaCfg -> Schema ()
fromCfg SchemaCfg{..} = sequenceS_
    [ mkBasicS orthS orthC
    , mkBasicS lemmaS lemmaC
    , mkBasicS shapeS shapeC
    , mkBasicS shapePairS shapePairC
    , mkBasicS suffixS suffixC
    , mkDictS dictC ]

-- | Use the schema to extract observations from the sentence.
schematize :: Schema a -> [Word] -> CRF.Sent Ob
schematize schema xs =
    map (S.fromList . Ox.execOx . schema v) [0 .. n - 1]
  where
    v = V.fromList xs
    n = V.length v
