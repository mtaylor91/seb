module Schemas.Schema
  ( Schema(..)
  , deriveSchema
  , validate
  ) where

import Data.Aeson.KeyMap (lookup, toList, fromList)
import Data.Bifunctor (second)
import Prelude hiding (lookup)
import qualified Data.Vector as V

import Schemas.Types


deriveSchema :: Node -> Schema
deriveSchema (ObjectNode obj) =
  RecordSchema (fromList (fmap (second deriveSchema) (toList obj)))
deriveSchema (ArrayNode arr) =
  let schemas = fmap deriveSchema arr
   in if all (== V.head schemas) schemas
    then ArraySchema (V.head schemas)
    else TupleSchema schemas
deriveSchema (StringNode _) = StringSchema
deriveSchema (NumberNode _) = NumberSchema
deriveSchema (BooleanNode _) = BooleanSchema
deriveSchema NullNode = NullSchema



validate :: Schema -> Node -> Bool
validate (MapSchema schema) (ObjectNode obj) = all (validate schema) obj
validate (ArraySchema schema) (ArrayNode arr) = all (validate schema) arr
validate (RecordSchema schema) (ObjectNode obj) =
  all (\(k, v) -> maybe False (`validate` v) (lookup k schema)) (toList obj)
validate StringSchema (StringNode _) = True
validate NumberSchema (NumberNode _) = True
validate BooleanSchema (BooleanNode _) = True
validate NullSchema NullNode = True
validate _ _ = False


_canonicalize :: Schema -> Schema
_canonicalize (MapSchema schema) =
  MapSchema (_canonicalize schema)
_canonicalize (ArraySchema schema) =
  ArraySchema (_canonicalize schema)
_canonicalize (RecordSchema schema) =
  RecordSchema (fromList (fmap (second _canonicalize) (toList schema)))
_canonicalize schema = schema
