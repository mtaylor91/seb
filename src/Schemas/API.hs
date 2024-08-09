{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Schemas.API
  ( SchemasAPI
  , SchemaAPI
  ) where

import Data.UUID (UUID)
import Servant

import Schemas.Types

type ListSchemas = Get '[JSON] [Schema]

type GetSchema = Get '[JSON] Schema

type PatchSchema = ReqBody '[JSON] Schema :> Patch '[JSON] Schema

type PutSchema = ReqBody '[JSON] Schema :> Put '[JSON] Schema

type DeleteSchema = Delete '[JSON] NoContent

type SchemaAPI = GetSchema :<|> PatchSchema :<|> PutSchema :<|> DeleteSchema

type SchemasAPI = "schemas" :> (ListSchemas :<|> Capture "id" UUID :> SchemaAPI)
