module Event
  ( Event
  , event
  , eventData
  , eventPublicKey
  , eventSignature
  , verifyEvent
  ) where

import Crypto.Sign.Ed25519 (PublicKey(..), Signature(..), dverify)
import Data.ByteString.Lazy as BSL
import Data.ByteString as BS

newtype Event = Event { _eventBytes :: BSL.ByteString }

event :: BSL.ByteString -> Event
event = Event

eventData :: Event -> BS.ByteString
eventData (Event bytes) = BSL.toStrict $ BSL.drop (32 + 64) bytes

eventPublicKey :: Event -> PublicKey
eventPublicKey (Event bytes) = PublicKey $ BSL.toStrict $ BSL.take 32 bytes

eventSignature :: Event -> Signature
eventSignature (Event bytes) = Signature $ BSL.toStrict $ BSL.take 64 $ BSL.drop 32 bytes

verifyEvent :: Event -> Bool
verifyEvent evt =
  dverify (eventPublicKey evt) (eventData evt) (eventSignature evt)
