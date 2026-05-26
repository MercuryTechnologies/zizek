module Hegel.Protocol.Error
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
  | CborDecodeFailure !Text !String -- context, decoder error
  | UnexpectedReply !Text !Value -- context, payload
  | MissingField !Text !Text -- context, field name
  | UnknownEvent !Text
  | ProtocolStateViolation !Text
  | HandshakeFailure !Text
  | VersionMismatch !Text !Text !Text -- got, lo, hi
  | StreamClosed
  deriving stock (Show)
  deriving anyclass (Exception)

-- | The server returned an application-level error response to a request.
-- Distinct from 'ProtocolError' because these are valid protocol messages,
-- not wire-format or state-machine violations.
data ServerError = ServerError
  { errorType :: !Text,
    errorPayload :: !Value
  }
  deriving stock (Show)
  deriving anyclass (Exception)
