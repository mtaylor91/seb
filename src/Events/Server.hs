{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Events.Server
  ( eventsServer
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson.KeyMap (union)
import Data.UUID (UUID)
import Servant

import Events.API
import Events.Store
import Events.Types

eventsServer :: EventsStore s => s -> Server EventsAPI
eventsServer s = listEvents s :<|> eventServer s

eventServer :: EventsStore s => s -> UUID -> Server EventAPI
eventServer s evtId
  = getEventHandler s evtId
  :<|> patchEventHandler s evtId
  :<|> putEventHandler s evtId
  :<|> deleteEventHandler s evtId

listEvents :: EventsStore s => s -> Handler [Event]
listEvents s = liftIO $ getEvents s

getEventHandler :: EventsStore s => s -> UUID -> Handler Event
getEventHandler s evtId = do
  maybe notFound pure =<< liftIO (getEvent s evtId)

patchEventHandler :: EventsStore s => s -> UUID -> Event -> Handler Event
patchEventHandler s evtId evt =
  if _eventId evt /= evtId
    then invalidId
    else liftIO (updateEvent s evtId patchEvt) >>= \case
      Just (Right evt') -> pure evt'
      Just (Left err) -> validationError err
      Nothing -> notFound
  where
    patchEvt prev =
      Right $ Event evtId $ _eventPayload evt `union` _eventPayload prev

putEventHandler :: EventsStore s => s -> UUID -> Event -> Handler Event
putEventHandler s evtId evt =
  if _eventId evt /= evtId
    then invalidId
    else liftIO (updateEvent s evtId (const (Right evt))) >>= \case
      Just (Right evt') -> pure evt'
      Just (Left err) -> validationError err
      Nothing -> notFound

deleteEventHandler :: EventsStore s => s -> UUID -> Handler NoContent
deleteEventHandler s evtId = do
  liftIO $ removeEvent s evtId
  pure NoContent

invalidId :: Handler a
invalidId = throwError err400 { errBody = "Invalid ID" }

notFound :: Handler a
notFound = throwError err404 { errBody = "Not found" }

validationError :: EventValidationError -> Handler a
validationError = \case
  InvalidId -> invalidId
