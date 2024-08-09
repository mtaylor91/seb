module Schemas.Store
  ( SchemaStore(..)
  ) where

import Data.UUID (UUID)

import Schemas.Types

class SchemaStore s where
  listSchemas :: s -> IO [Schema]
  getSchema :: s -> UUID -> IO (Maybe Schema)
  setSchema :: s -> UUID -> Schema -> IO ()
  deleteSchema :: s -> UUID -> IO ()
