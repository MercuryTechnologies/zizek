-- | Exceptions raised by the @hegel@ server wire-protocol layer.
module Hegel.Server.Protocol.Error
  ( ProtocolError (..),
    ConnectionClosedError (..),
    ServerError (..),
  )
where

import CBOR.Value (Value)
import Control.Exception (Exception)
import Data.Text (Text)
import Data.Word (Word32)

-- | Remote closed the pipe (EOF / broken pipe during readPacket).
data ConnectionClosedError = ConnectionClosedError !Text
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Wire-format and state-machine violations.
data ProtocolError
  = BadMagic !Word32
  | ChecksumMismatch
  | BadTerminator
  | CborDecodeFailure !Text !String
  | UnexpectedReply !Text !Value
  | MissingField !Text !Text
  | UnknownEvent !Text
  | ProtocolStateViolation !Text
  | HandshakeFailure !Text
  | VersionMismatch !Text !Text !Text
  | StreamClosed
  deriving stock (Show)
  deriving anyclass (Exception)

-- | The server returned an application-level error response.
data ServerError = ServerError
  { errorType :: !Text,
    errorPayload :: !Value
  }
  deriving stock (Show)
  deriving anyclass (Exception)
