{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Events.API
import Events.Server
import Events.State
import Events.Store

import Schemas.API
import Schemas.Server
import Schemas.State
import Schemas.Store

type API = EventsAPI :<|> SchemasAPI

startApp :: IO ()
startApp = do
  es <- eventsStateStore
  ss <- schemaStateStore
  run 8080 $ app es ss

app :: (EventsStore es, SchemaStore ss) => es -> ss -> Application
app es ss = serve api $ server es ss

api :: Proxy API
api = Proxy

server :: (EventsStore es, SchemaStore ss) => es -> ss -> Server API
server es ss = eventsServer es :<|> schemasServer ss
