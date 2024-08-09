module Event.Router.State
  ( EventRouterState
  , eventRouterState
  ) where

import Control.Concurrent.STM
import Crypto.Sign.Ed25519
import Data.Map.Strict
import Data.UUID

import Event

data EventRouterState = EventRouterState
  { _handlers :: Map UUID EventRouterStateHandler
  , _destinations :: Map PublicKey EventRouterStateDestination
  }

data EventRouterStateHandler = EventRouterStateHandler
  { _handler :: Event -> STM ()
  , _handlerDestinations :: Maybe [PublicKey]
  }

newtype EventRouterStateDestination = EventRouterStateDestination { _destinationHandlers :: [UUID] }

eventRouterState :: EventRouterState
eventRouterState = EventRouterState empty empty
