module Schemas.Server
  ( schemasServer
  ) where


import Control.Monad.IO.Class (liftIO)
import Data.UUID (UUID)
import Servant

import Schemas.API
import Schemas.Store
import Schemas.Types

schemasServer :: SchemaStore s => s -> Server SchemasAPI
schemasServer s = listSchemasHandler s :<|> schemaServer s

schemaServer :: SchemaStore s => s -> UUID -> Server SchemaAPI
schemaServer s schemaId
  = getSchemaHandler s schemaId
  :<|> patchSchemaHandler s schemaId
  :<|> putSchemaHandler s schemaId
  :<|> deleteSchemaHandler s schemaId


listSchemasHandler :: SchemaStore s => s -> Handler [Schema]
listSchemasHandler s = liftIO $ listSchemas s


getSchemaHandler :: SchemaStore s => s -> UUID -> Handler Schema
getSchemaHandler s schemaId = do
  maybe notFound pure =<< liftIO (getSchema s schemaId)


patchSchemaHandler :: SchemaStore s => s -> UUID -> Schema -> Handler Schema
patchSchemaHandler s schemaId schema =
  liftIO (setSchema s schemaId schema) >> pure schema


putSchemaHandler :: SchemaStore s => s -> UUID -> Schema -> Handler Schema
putSchemaHandler s schemaId schema =
  liftIO (setSchema s schemaId schema) >> pure schema


deleteSchemaHandler :: SchemaStore s => s -> UUID -> Handler NoContent
deleteSchemaHandler s schemaId = do
  liftIO $ deleteSchema s schemaId
  pure NoContent


notFound :: Handler a
notFound = throwError err404
