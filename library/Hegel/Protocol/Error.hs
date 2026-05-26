module Hegel.Protocol.Error
  ( ProtocolError (..),
    ConnectionClosedError (..),
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
  | RequestError !Text !Text !Value -- context, errorType, payload
  deriving stock (Show)
  deriving anyclass (Exception)
