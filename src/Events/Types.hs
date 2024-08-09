{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Events.Types
  ( Event(..)
  , EventData(..)
  , EventValidationError(..)
  , EventUpdate, EventUpdateResult
  , parseEventData
  ) where

import Data.Aeson
import Data.Aeson.KeyMap (KeyMap, insert)
import Data.ByteString (ByteString)
import Data.UUID (UUID)

data Event = Event
  { _eventId            :: UUID
  , _eventPayload       :: KeyMap Value
  } deriving (Eq, Show)

instance FromJSON Event where
  parseJSON (Object v) = Event <$> v .: "id" <*> pure v
  parseJSON _ = fail "Expected an object"

instance ToJSON Event where
  toJSON Event{..} = Object $ insert "id" (toJSON _eventId) _eventPayload


data EventData = EventData
  { _event      :: Event
  , _eventBytes :: ByteString
  } deriving (Eq, Show)

parseEventData :: ByteString -> Maybe EventData
parseEventData bs = do
  event <- decodeStrict bs
  pure $ EventData event bs


data EventValidationError
  = InvalidId
  deriving (Eq, Show)


type EventUpdate = Event -> EventUpdateResult

type EventUpdateResult = Either EventValidationError Event
