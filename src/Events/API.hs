{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Events.API
  ( EventsAPI
  , EventAPI
  ) where

import Data.UUID (UUID)
import Servant

import Events.Types

type ListEvents = Get '[JSON] [Event]

type GetEvent = Get '[JSON] Event

type PatchEvent = ReqBody '[JSON] Event :> Patch '[JSON] Event

type PutEvent = ReqBody '[JSON] Event :> Put '[JSON] Event

type DeleteEvent = Delete '[JSON] NoContent

type EventAPI = GetEvent :<|> PatchEvent :<|> PutEvent :<|> DeleteEvent

type EventsAPI = "events" :> (ListEvents :<|> Capture "id" UUID :> EventAPI)
