{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( runServer
    ) where

import Crypto.Sign.Ed25519 (PublicKey(..))
import Data.Binary.Builder
import Data.ByteString (ByteString)
import Data.ByteString.Base64
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (Port, run)

import Event
import Event.Router

app :: EventRouter -> Application
app router req respond = do

  case requestMethod req of
    "GET" -> handleGet
    "POST" -> handlePost
    _ -> respond $ responseLBS status405 [] "Method Not Allowed"

  where

  handleGet = do
    case pathInfoToPublicKey $ pathInfo req of
      Nothing -> respond $ responseLBS status400 [] "Bad Request"
      Just publicKeyBytes -> do
        let publicKey = PublicKey publicKeyBytes
        respond $ responseStream status200 [] $ subscribeF publicKey

  handlePost = do
    body <- strictRequestBody req
    let evt = event body
    routeEvent router evt
    respond $ responseLBS status200 [] "OK"

  subscribeF :: PublicKey -> (Builder -> IO ()) -> IO () -> IO ()
  subscribeF publicKey writeStream flushStream = do
    subscribeEvents router publicKey $ \evt -> do
      writeStream $ fromByteString $ eventData evt
      flushStream
      return True

pathInfoToPublicKey :: [Text] -> Maybe ByteString
pathInfoToPublicKey [x] =
  case decodeBase64 $ encodeUtf8 x of
    Left _ -> Nothing
    Right bs -> Just bs
pathInfoToPublicKey _ = Nothing

runServer :: Port -> IO ()
runServer port = do
  router <- eventRouter
  run port $ app router
