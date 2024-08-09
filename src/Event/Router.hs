module Event.Router
  ( EventRouter
  , eventRouter
  , routeEvent
  , subscribeEvents
  ) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM
import Control.Monad (filterM, forever, when)
import Crypto.Sign.Ed25519 (PublicKey(..))
import Data.Map.Strict (Map, empty, findWithDefault, insert, insertWith)
import Event (Event, eventPublicKey, verifyEvent)

type EventHandler = Event -> STM Bool

data EventRouter = EventRouter
  { _eventHandler :: PublicKey -> EventHandler -> STM ()
  , _routeEvent :: Event -> STM ()
  , _threadId :: ThreadId
  }

eventRouter :: IO EventRouter
eventRouter = do
  newEvent <- newEmptyTMVarIO
  newHandler <- newEmptyTMVarIO
  handlers <- newTVarIO empty

  let eventHandler = handleEvent handlers <$> takeTMVar newEvent
      handlerHandler = handleHandler handlers <$> takeTMVar newHandler

  threadId <- forkIO $ forever $ atomically $ eventHandler `orElse` handlerHandler

  return $ EventRouter
    { _eventHandler = curry $ putTMVar newHandler
    , _routeEvent = putTMVar newEvent
    , _threadId = threadId
    }

handleEvent ::
  TVar (Map PublicKey [EventHandler]) -> Event -> STM ()
handleEvent handlers evt = do
  let pk = eventPublicKey evt
  handlers' <- readTVar handlers
  hs <- filterM ($ evt) $ findWithDefault [] pk handlers'
  modifyTVar' handlers $ insert pk hs


handleHandler ::
  TVar (Map PublicKey [EventHandler]) -> (PublicKey, EventHandler) -> STM ()
handleHandler handlers (pk, handler) =
  modifyTVar' handlers $ insertWith (++) pk [handler]

routeEvent :: EventRouter -> Event -> IO ()
routeEvent router evt = do
  if verifyEvent evt
    then atomically $ _routeEvent router evt
    else putStrLn "Event failed verification"

subscribeEvents :: EventRouter -> PublicKey -> (Event -> IO Bool) -> IO ()
subscribeEvents router pk handler = do
  eventVar <- newEmptyTMVarIO
  continueVar <- newTVarIO True
  atomically $ _eventHandler router pk $ handleSTM eventVar continueVar
  subscribeLoop eventVar continueVar

  where

    handleSTM :: TMVar Event -> TVar Bool -> EventHandler
    handleSTM eventVar continueVar evt = do
      continue <- readTVar continueVar
      if continue
        then putTMVar eventVar evt >> return True
        else return False
    
    subscribeLoop :: TMVar Event -> TVar Bool -> IO ()
    subscribeLoop eventVar continueVar = do
      evt <- atomically $ takeTMVar eventVar
      continue <- handler evt
      atomically $ writeTVar continueVar continue
      when continue $ subscribeLoop eventVar continueVar
