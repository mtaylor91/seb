module Events.State
  ( eventsStateStore
  ) where

import Control.Concurrent.STM
import Data.HashMap.Strict hiding (map)
import Data.UUID (UUID)
import Prelude hiding (lookup)

import Events.Store
import Events.Types


newtype EventsState = EventsState (HashMap UUID Event)
  deriving (Eq, Show)

instance Semigroup EventsState where
  (EventsState a) <> (EventsState b) = EventsState (a <> b)

instance Monoid EventsState where
  mempty = EventsState mempty
  mappend = (<>)


newtype EventsStateStore = EventsStateStore (TVar EventsState)

instance EventsStore EventsStateStore where
  getEvents (EventsStateStore tvar) = do
    EventsState events <- readTVarIO tvar
    return (elems events)

  getEvent (EventsStateStore tvar) uuid = do
    EventsState events <- readTVarIO tvar
    return (lookup uuid events)

  addEvent (EventsStateStore tvar) event = do
    atomically $ do
      EventsState events <- readTVar tvar
      writeTVar tvar (EventsState (insert (_eventId event) event events))

  removeEvent (EventsStateStore tvar) uuid = do
    atomically $ do
      EventsState events <- readTVar tvar
      writeTVar tvar (EventsState (delete uuid events))

  updateEvent (EventsStateStore tvar) uuid f = do
    atomically $ do
      EventsState events <- readTVar tvar
      case lookup uuid events of
        Nothing -> return Nothing
        Just event ->
          case f event of
            Left err ->
              return (Just (Left err))
            Right event' -> do
              writeTVar tvar (EventsState (insert uuid event' events))
              return (Just (Right event'))


eventsStateStore :: IO EventsStateStore
eventsStateStore = do
  tvar <- newTVarIO mempty
  return (EventsStateStore tvar)
