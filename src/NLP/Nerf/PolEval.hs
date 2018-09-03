-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}


module NLP.Nerf.PolEval
  ( processJSON
  ) where


import Prelude hiding (id)
-- import GHC.Generics

import           Control.Monad (mzero)
import           Control.Applicative ((<$>), (<*>), pure)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Tree as R
-- import qualified Data.Text.Lazy as L
import qualified Data.Aeson as JSON
import           Data.Aeson ((.:), (.=))

import qualified NLP.Nerf as Nerf
import qualified NLP.Nerf.Offset as Off


data Input = Input
  { text :: T.Text
  , id  :: T.Text
  , answers :: T.Text
  } deriving (Show)



instance JSON.FromJSON Input where
    parseJSON (JSON.Object v) =
      Input
      <$> v .: "text"
      <*> v .: "id"
      <*> pure ""
    parseJSON _ = mzero


-- instance ToJSON Person where
--   toJSON (Person name age) = object ["name" .= name, "age" .= age]

instance JSON.ToJSON Input where
    toJSON Input{..} =
      JSON.object
      [ "text" .= text
      , "id" .= id
      , "answers" .= answers
      ]


-- | Process input.
processJSON
  :: Nerf.Nerf
  -- ^ Model
  -> BL.ByteString
  -- ^ The encoded JSON to process
  -> BL.ByteString
  -- ^ Processed JSON
processJSON nerf inpTxt =
  case JSON.decode inpTxt of
    Nothing -> error "PolEval: failed to decode JSON"
    Just inputs -> JSON.encode (map process inputs)
  where
    dot2underscore = T.map $ \case
      '.' -> '_'
      c -> c
    process inp@Input{..} =
      let forest
            = concatMap (Nerf.ner nerf . T.unpack)
            . T.splitOn "\n\n"
            $ text
          forest' = Off.addOffsetInfo text forest
          ans = T.unlines
            [ T.concat
              [ dot2underscore ann
              , " "
              , T.pack $ show beg
              , " "
              , T.pack $ show end
              , "\t"
              , T.take (end - beg) (T.drop beg text)
              ]
            | tree <- forest'
            , Left Off.Offset{..} <- R.flatten tree
            ]
      in  inp {answers = ans}


-- -- | Process input.
-- processJSON
--   :: Nerf.Nerf
--   -- ^ Model
--   -> BL.ByteString
--   -- ^ The encoded JSON to process
--   -> BL.ByteString
--   -- ^ Processed JSON
-- processJSON nerf inpTxt =
--   case JSON.decode inpTxt of
--     Nothing -> error "PolEval: failed to decode JSON"
--     Just inputs -> JSON.encode (map process inputs)
--   where
--     dot2underscore = T.map $ \case
--       '.' -> '_'
--       c -> c
--     process inp@Input{..} =
--       let forest = Nerf.ner nerf (T.unpack text)
--           forest' = Off.addOffsetInfo text forest
--           ans = T.unlines
--             [ T.concat
--               [ dot2underscore ann
--               , " "
--               , T.pack $ show beg
--               , " "
--               , T.pack $ show end
--               , "\t"
--               , T.take (end - beg) (T.drop beg text)
--               ]
--             | tree <- forest'
--             , Left Off.Offset{..} <- R.flatten tree
--             ]
--       in  inp {answers = ans}
