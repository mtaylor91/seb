module Schemas.State
  ( SchemaStateStore(..)
  , schemaStateStore
  ) where

import Control.Concurrent.STM
import Data.HashMap.Strict hiding (map)
import Data.UUID (UUID)
import Prelude hiding (lookup)

import Schemas.Store
import Schemas.Types

newtype SchemaStateStore = SchemaStateStore (TVar (HashMap UUID Schema))


schemaStateStore :: IO SchemaStateStore
schemaStateStore = do
  tvar <- newTVarIO mempty
  return (SchemaStateStore tvar)


instance SchemaStore SchemaStateStore where

  listSchemas (SchemaStateStore tvar) =
    elems <$> readTVarIO tvar

  getSchema (SchemaStateStore tvar) uuid =
    lookup uuid <$> readTVarIO tvar

  setSchema (SchemaStateStore tvar) uuid schema =
    atomically $ modifyTVar' tvar (insert uuid schema)

  deleteSchema (SchemaStateStore tvar) uuid =
    atomically $ modifyTVar' tvar (delete uuid)
