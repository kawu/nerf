{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Observation schema blocks for Nerf.

module NLP.Nerf.Schema
( 
-- * Types
  Ox
, Schema
, void
, sequenceS_

-- * Usage
, schematize

-- ** Configuration
, Body (..)
, Entry
, entry
, entryWith
, SchemaConf (..)
, nullConf
, defaultConf
, fromConf

-- ** Schema blocks
, Block
, fromBlock
, orthB
, splitOrthB
, lowPrefixesB
, lowSuffixesB
, lemmaB
, shapeB
, packedB
, shapePairB
, packedPairB
, dictB
) where

import           Prelude hiding (Word)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, join)
import Data.Maybe (maybeToList)
import Data.Binary (Binary, put, get)
import qualified Data.Char as C
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.DAWG.Static as D

import qualified Data.CRF.Chain1 as CRF
import qualified Control.Monad.Ox as Ox
import qualified Control.Monad.Ox.Text as Ox

import NLP.Nerf.Types
import NLP.Nerf.Dict (Dict)

-- | The Ox monad specialized to word token type and text observations.
type Ox a = Ox.Ox Word T.Text a

-- | A schema is a block of the Ox computation performed within the
-- context of the sentence and the absolute sentence position.
type Schema a = V.Vector Word -> Int -> Ox a

-- | A dummy schema block.
void :: a -> Schema a
void x _ _ = return x

-- | Sequence the list of schemas (or blocks) and discard individual values.
sequenceS_ :: [V.Vector Word -> a -> Ox b] -> V.Vector Word -> a -> Ox ()
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

-- | Transform the block to the schema depending on the list of
-- relative sentence positions.
fromBlock :: Block a -> [Int] -> Schema a
fromBlock blk xs sent =
    let blkSent = blk sent
    in  \k -> blkSent [x + k | x <- xs]

-- | Orthographic form at the current position.
orthB :: Block ()
orthB sent = \ks ->
    let orthOb = Ox.atWith sent id
    in  mapM_ (Ox.save . orthOb) ks

-- | Orthographic form split into two observations: the lowercased form and
-- the original form (only when different than the lowercased one).
splitOrthB :: Block ()
splitOrthB sent = \ks -> do
    mapM_ (Ox.save . lowOrth)    ks
    mapM_ (Ox.save . upOnlyOrth) ks
  where
    BaseOb{..}      = mkBaseOb sent
    upOnlyOrth i    = orth i >>= \x -> case T.any C.isUpper x of
        True    -> Just x
        False   -> Nothing

-- | List of lowercased prefixes of given lengths.
lowPrefixesB :: [Int] -> Block ()
lowPrefixesB ns sent = \ks ->
    forM_ ks $ \i ->
        mapM_ (Ox.save . lowPrefix i) ns
  where
    BaseOb{..}      = mkBaseOb sent
    lowPrefix i j   = Ox.prefix j =<< lowOrth i

-- | List of lowercased suffixes of given lengths.
lowSuffixesB :: [Int] -> Block ()
lowSuffixesB ns sent = \ks ->
    forM_ ks $ \i ->
        mapM_ (Ox.save . lowSuffix i) ns
  where
    BaseOb{..}      = mkBaseOb sent
    lowSuffix i j   = Ox.suffix j =<< lowOrth i

-- | Lemma substitute parametrized by the number specifying the span
-- over which lowercased prefixes and suffixes will be 'Ox.save'd.
-- For example, @lemmaB 2@ will take affixes of lengths @0, -1@ and @-2@
-- on account.
lemmaB :: Int -> Block ()
lemmaB n sent = \ks -> do
    mapM_ lowLemma ks
  where
    BaseOb{..}      = mkBaseOb sent
    lowPrefix i j   = Ox.prefix j =<< lowOrth i
    lowSuffix i j   = Ox.suffix j =<< lowOrth i
    lowLemma i = Ox.group $ do
        mapM_ (Ox.save . lowPrefix i) [0, -1 .. -n]
        mapM_ (Ox.save . lowSuffix i) [0, -1 .. -n]

-- | Shape of the word.
shapeB :: Block ()
shapeB sent = \ks -> do
    mapM_ (Ox.save . shape) ks
  where
    BaseOb{..}      = mkBaseOb sent
    shape i         = Ox.shape <$> orth i

-- | Packed shape of the word.
packedB :: Block ()
packedB sent = \ks -> do
    mapM_ (Ox.save . shapeP) ks
  where
    BaseOb{..}      = mkBaseOb sent
    shape i         = Ox.shape <$> orth i
    shapeP i        = Ox.pack <$> shape i

-- -- | Shape and packed shape of the word.
-- shapeAndPackedB :: Block ()
-- shapeAndPackedB sent = \ks -> do
--     mapM_ (Ox.save . shape)  ks
--     mapM_ (Ox.save . shapeP) ks
--   where
--     BaseOb{..}      = mkBaseOb sent
--     shape i         = Ox.shape <$> orth i
--     shapeP i        = Ox.pack <$> shape i

-- | Combined shapes of two consecutive (at @k-1@ and @k@ positions) words.
shapePairB :: Block ()
shapePairB sent = \ks ->
    forM_ ks $ \i -> do
        Ox.save $ link <$> shape  i <*> shape  (i - 1)
  where
    BaseOb{..}      = mkBaseOb sent
    shape i         = Ox.shape <$> orth i
    link x y        = T.concat [x, "-", y]

-- | Combined packed shapes of two consecutive (at @k-1@ and @k@ positions)
-- words.
packedPairB :: Block ()
packedPairB sent = \ks ->
    forM_ ks $ \i -> do
        Ox.save $ link <$> shapeP i <*> shapeP (i - 1)
  where
    BaseOb{..}      = mkBaseOb sent
    shape i         = Ox.shape <$> orth i
    shapeP i        = Ox.pack <$> shape i
    link x y        = T.concat [x, "-", y]

-- | Plain dictionary search determined with respect to the list of
-- relative positions.
dictB :: Dict -> Block ()
dictB dict sent = \ks -> do
    mapM_ (Ox.saves . searchDict) ks
  where
    BaseOb{..}      = mkBaseOb sent
    searchDict i    = join . maybeToList $
        S.toList <$> (orth i >>= flip D.lookup dict . T.unpack)

-- | Body of configuration entry.
data Body a = Body {
    -- | Range argument for the schema block. 
      range :: [Int]
    -- | Additional arguments for the schema block.
    , args  :: a }
    deriving (Show)

instance Binary a => Binary (Body a) where
    put Body{..} = put range >> put args
    get = Body <$> get <*> get

-- | Maybe entry.
type Entry a = Maybe (Body a)

-- | Entry with additional arguemnts.
entryWith :: a -> [Int] -> Entry a
entryWith v xs = Just (Body xs v)

-- | Maybe entry with additional arguemnts.
entryWith'Mb :: Maybe a -> [Int] -> Entry a
entryWith'Mb (Just v) xs = Just (Body xs v)
entryWith'Mb Nothing _   = Nothing

-- | Plain entry with no additional arugments.
entry :: [Int] -> Entry ()
entry = entryWith ()

-- | Configuration of the schema.  All configuration elements specify the
-- range over which a particular observation type should be taken on account.
-- For example, the @[-1, 0, 2]@ range means that observations of particular
-- type will be extracted with respect to previous (@k - 1@), current (@k@)
-- and after the next (@k + 2@) positions when identifying the observation
-- set for position @k@ in the input sentence.
data SchemaConf = SchemaConf {
    -- | The 'orthB' schema block.
      orthC             :: Entry ()
    -- | The 'splitOrthB' schema block.
    , splitOrthC        :: Entry ()
    -- | The 'lowPrefixesB' schema block.  The first list of ints
    -- represents lengths of prefixes.
    , lowPrefixesC      :: Entry [Int]
    -- | The 'lowSuffixesB' schema block.  The first list of ints
    -- represents lengths of suffixes.
    , lowSuffixesC      :: Entry [Int]
    -- | The 'lemmaB' schema block.
    , lemmaC            :: Entry Int
    -- | The 'shapeB' schema block.
    , shapeC            :: Entry ()
    -- | The 'packedB' schema block.
    , packedC           :: Entry ()
    -- | The 'shapePairB' schema block.
    , shapePairC        :: Entry ()
    -- | The 'packedPairB' schema block.
    , packedPairC       :: Entry ()
    -- | Dictionaries of NEs ('dictB' schema block).
    , dictC             :: Entry [Dict]
    -- | Dictionary of internal triggers.
    , intTrigsC         :: Entry Dict
    -- | Dictionary of external triggers.
    , extTrigsC         :: Entry Dict
    } deriving (Show)

instance Binary SchemaConf where
    put SchemaConf{..} = do
        put orthC
        put splitOrthC
        put lowPrefixesC
        put lowSuffixesC
        put lemmaC
        put shapeC
        put packedC
        put shapePairC
        put packedPairC
        put dictC
        put intTrigsC
        put extTrigsC
    get = SchemaConf
        <$> get <*> get <*> get <*> get
        <*> get <*> get <*> get <*> get
        <*> get <*> get <*> get <*> get

-- | Null configuration of the observation schema.
nullConf :: SchemaConf
nullConf = SchemaConf
    Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing

-- | Default configuration of the observation schema.
defaultConf
    :: [Dict]       -- ^ Named Entity dictionaries
    -> Maybe Dict   -- ^ Dictionary of internal triggers
    -> Maybe Dict   -- ^ Dictionary of external triggers
    -> IO SchemaConf
defaultConf neDicts intDict extDict = do
    return $ SchemaConf
        { orthC         = Nothing
        , splitOrthC    = entry                 [-1, 0]
        , lowPrefixesC  = Nothing
        , lowSuffixesC  = entryWith [2, 3, 4]   [0]
        , lemmaC        = entryWith 3           [-1, 0]
        , shapeC        = entry                 [-1, 0]
        , packedC       = entry                 [-1, 0]
        , shapePairC    = entry                 [0]
        , packedPairC   = entry                 [0]
        , dictC         = entryWith neDicts     [-1, 0]
        , intTrigsC     = entryWith'Mb intDict  [0]
        , extTrigsC     = entryWith'Mb extDict  [-1] }

mkArg0 :: Block () -> Entry () -> Schema ()
mkArg0 blk (Just x) = fromBlock blk (range x)
mkArg0 _   Nothing  = void ()

mkArg1 :: (a -> Block ()) -> Entry a -> Schema ()
mkArg1 blk (Just x) = fromBlock (blk (args x)) (range x)
mkArg1 _   Nothing  = void ()

mkArgs1 :: (a -> Block ()) -> Entry [a] -> Schema ()
mkArgs1 blk (Just x) = sequenceS_
    [ fromBlock
        (blk dict)
        (range x)
    | dict <- args x ]
mkArgs1 _   Nothing  = void ()

-- | Build the schema based on the configuration.
fromConf :: SchemaConf -> Schema ()
fromConf SchemaConf{..} = sequenceS_
    [ mkArg0 orthB orthC
    , mkArg0 splitOrthB splitOrthC
    , mkArg1 lowPrefixesB lowPrefixesC
    , mkArg1 lowSuffixesB lowSuffixesC
    , mkArg1 lemmaB lemmaC
    , mkArg0 shapeB shapeC
    , mkArg0 packedB packedC
    , mkArg0 shapePairB shapePairC
    , mkArg0 packedPairB packedPairC
    , mkArgs1 dictB dictC
    , mkArg1 dictB intTrigsC
    , mkArg1 dictB extTrigsC ]

-- | Use the schema to extract observations from the sentence.
schematize :: Schema a -> [Word] -> CRF.Sent Ob
schematize schema xs =
    map (S.fromList . Ox.execOx . schema v) [0 .. n - 1]
  where
    v = V.fromList xs
    n = V.length v
