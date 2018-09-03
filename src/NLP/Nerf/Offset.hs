{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}


-- | Offset-based data format.


module NLP.Nerf.Offset
  ( addOffsetInfo
  , Offset(..)
  ) where


import           Control.Monad (forM, forM_)
import qualified Control.Monad.State.Strict as ST
import qualified Data.Traversable as Tr
import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.Tree as R

import qualified Data.Named.Tree as N


type Token = Offset T.Text


data Offset a = Offset
  { beg :: Int
  , end :: Int
  , ann :: a
  } deriving (Show, Eq, Ord)


-- -- | An alternative version of `addOffsetInfo` which interprets double newline
-- -- characters as separators between paragraphs. Paragraphs are processed
-- -- separately and information about offsets is (supposed to be) handled
-- -- accordingly.
-- addOffsetInfo'
--   :: T.Text
--      -- ^ The original sentence
--   -> N.NeForest a T.Text
--      -- ^ The sentence with NE annotations
--   -> N.NeForest (Offset a) Token
-- addOffsetInfo' sent ts
--   = pullOffsetInfo
--   . flip ST.evalState (stateInit sent)
--   . forM ts . Tr.mapM $ \e -> case e of
--     Left x  -> return (Left x)
--     Right x -> do
--       tok <- consume x
--       return $ Right tok


-- | Add information about offsets to the individual tokens.
addOffsetInfo
  :: T.Text
     -- ^ The original sentence
  -> N.NeForest a T.Text
     -- ^ The sentence with NE annotations
  -> N.NeForest (Offset a) Token
addOffsetInfo sent ts
  = pullOffsetInfo
  . flip ST.evalState (stateInit sent)
  . forM ts . Tr.mapM $ \e -> case e of
    Left x  -> return (Left x)
    Right x -> do
      tok <- consume x
      return $ Right tok


-- | Pull information about offsets to NE annotations.
pullOffsetInfo
  :: N.NeForest a Token
  -> N.NeForest (Offset a) Token
pullOffsetInfo =
  map pullOffset
  where
    pullOffset t = case R.rootLabel t of
      Left x ->
        let children = map pullOffset $ R.subForest t
            p = minimum $ map (begOffset . R.rootLabel) children
            q = maximum $ map (endOffset . R.rootLabel) children
            off = Offset
              { beg = p
              , end = q
              , ann = x
              }
        in  R.Node (Left off) children
      Right x | null (R.subForest t) -> R.Node (Right x) []
      _ -> error "Offset.pullOffsetInfo: impossible happened"
    begOffset = \case
      Left x -> beg x
      Right x -> beg x
    endOffset = \case
      Left x -> end x
      Right x -> end x


--------------------------
-- Offset computing monad
--------------------------


-- Offset monad
type Off a = ST.State State a


data State = State
  { rest :: T.Text
    -- ^ The remaining part of the sentence
  , offset :: Int
    -- ^ The offset so far
  }


-- | Initial state
stateInit :: T.Text -> State
stateInit x = State
  { rest = x
  , offset = 0
  }


-- | Consume a token
consume
  :: T.Text
     -- ^ Word to consume
  -> Off Token
consume x = do
  consumeWhiteSpaces
  p <- ST.gets offset
  consumeText x
  q <- ST.gets offset
  return $ Offset
    { beg = p
    , end = q
    , ann = x
    }


-- | Consume a piece of text, possibly with whitespace characters inside.
consumeText :: T.Text -> Off ()
consumeText xs = do
  forM_ (T.words xs) $ \x -> do
    consumeWhiteSpaces
    consumeAtom x


-- | Consume a piece of text, with *no* whitespace characters inside.
consumeAtom :: T.Text -> Off ()
consumeAtom atom = do
  State{..} <- ST.get
  case T.stripPrefix atom rest of
    Nothing -> error $
      "Offset: " ++
      T.unpack atom ++
      " <not a prefix of> " ++
      T.unpack rest
    Just rest' -> do
      ST.put $ State
        { rest = rest'
        , offset = offset + T.length atom
        }


-- | Consume any number of whitespace characters.
consumeWhiteSpaces :: Off ()
consumeWhiteSpaces = do
  b <- consumeWhiteSpace
  if b
    then consumeWhiteSpaces
    else return ()
  where
    consumeWhiteSpace = do
      State{..} <- ST.get
      case T.uncons rest of
        Nothing -> return False
        Just (c, cs) -> do
          if C.isSpace c
            then do
              ST.put $ State cs (offset + 1)
              return True
            else do
              return False
