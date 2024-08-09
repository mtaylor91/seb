{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Schemas.Types
  ( Node(..)
  , Schema(..)
  ) where

import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Scientific (Scientific)
import Data.Text
import Data.Vector (Vector)


data Node where
  ObjectNode :: KeyMap Node -> Node
  ArrayNode :: Vector Node -> Node
  StringNode :: Text -> Node
  NumberNode :: Scientific -> Node
  BooleanNode :: Bool -> Node
  NullNode :: Node
  deriving (Eq, Show)


data Schema where
  MapSchema :: Schema -> Schema
  ArraySchema :: Schema -> Schema
  TupleSchema :: Vector Schema -> Schema
  RecordSchema :: KeyMap Schema -> Schema
  StringSchema :: Schema
  NumberSchema :: Schema
  BooleanSchema :: Schema
  NullSchema :: Schema
  UnionSchema :: Vector Schema -> Schema
  deriving (Eq, Show)

instance FromJSON Schema where
  parseJSON (Object o) = do
    t :: Maybe Text <- o .:? "type"
    case t of
      Just "object" -> do
        properties <- o .:? "properties"
        case properties of
          Just s -> RecordSchema <$> parseJSON s
          Nothing -> MapSchema <$> o .: "additionalProperties"
      Just "array" -> do
        items <- o .:? "items"
        case items of
          Just s -> ArraySchema <$> parseJSON s
          Nothing -> TupleSchema <$> o .: "prefixItems"
      Just "string" -> return StringSchema
      Just "number" -> return NumberSchema
      Just "boolean" -> return BooleanSchema
      Just "null" -> return NullSchema
      Just _ -> fail "Invalid schema type"
      Nothing -> do
        anyOf <- o .:? "anyOf"
        case anyOf of
          Just s -> UnionSchema <$> parseJSON s
          Nothing -> fail "Invalid schema"
  parseJSON _ = fail "Invalid schema"


instance ToJSON Schema where
  toJSON (MapSchema s) = object ["type" .= String "object", "additionalProperties" .= s]
  toJSON (ArraySchema s) = object ["type" .= String "array", "items" .= s]
  toJSON (TupleSchema s) = object ["type" .= String "array", "prefixItems" .= s]
  toJSON (RecordSchema s) = object ["type" .= String "object", "properties" .= s]
  toJSON StringSchema = object ["type" .= String "string"]
  toJSON NumberSchema = object ["type" .= String "number"]
  toJSON BooleanSchema = object ["type" .= String "boolean"]
  toJSON NullSchema = object ["type" .= String "null"]
  toJSON (UnionSchema s) = object ["anyOf" .= s]
