module Events.Store
  ( EventsStore(..)
  ) where

import Data.UUID (UUID)

import Events.Types


class EventsStore s where
  getEvents :: s -> IO [Event]
  getEvent :: s -> UUID -> IO (Maybe Event)
  addEvent :: s -> Event -> IO ()
  removeEvent :: s -> UUID -> IO ()
  updateEvent :: s -> UUID -> EventUpdate -> IO (Maybe EventUpdateResult)
