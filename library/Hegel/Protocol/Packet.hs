{-# LANGUAGE ViewPatterns #-}

module Hegel.Protocol.Packet
  ( Packet (..),
    readPacket,
    writePacket,
  )
where

import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Digest.CRC32 (crc32)
import Data.Word (Word32, Word8)
import Hegel.Protocol.Error (ProtocolError (..))
import System.IO (Handle, hFlush)
import UnliftIO.Exception (throwIO)

pattern Magic :: Word32
pattern Magic = 0x4845474C

pattern Terminator :: Word8
pattern Terminator = 0x0A

pattern ReplyBit :: Word32
pattern ReplyBit = 0x80000000

packetHeaderSize :: Int
packetHeaderSize = 20

data Packet = Packet
  { stream :: !Word32,
    messageId :: !Word32,
    isReply :: !Bool,
    payload :: !ByteString
  }
  deriving stock (Show)

w32be :: Word32 -> [Word8]
w32be w =
  [ fromIntegral (w `shiftR` 24),
    fromIntegral (w `shiftR` 16 .&. 0xFF),
    fromIntegral (w `shiftR` 8 .&. 0xFF),
    fromIntegral (w .&. 0xFF)
  ]

beWord32 :: ByteString -> Int -> Word32
beWord32 bs i =
  fromIntegral (BS.index bs i) `shiftL` 24
    .|. fromIntegral (BS.index bs (i + 1)) `shiftL` 16
    .|. fromIntegral (BS.index bs (i + 2)) `shiftL` 8
    .|. fromIntegral (BS.index bs (i + 3))

-- | View function: parse a 20-byte header.
-- Succeeds only if magic matches.
parseHeader :: ByteString -> Maybe (Word32, Word32, Word32, Int)
parseHeader hdr
  | BS.length hdr < packetHeaderSize = Nothing
  | beWord32 hdr 0 /= Magic = Nothing
  | otherwise =
      Just
        ( beWord32 hdr 4, -- stored checksum
          beWord32 hdr 8, -- stream id
          beWord32 hdr 12, -- raw message id (reply bit intact)
          fromIntegral (beWord32 hdr 16) -- payload length
        )

pattern ValidHeader ::
  Word32 -> -- stored checksum
  Word32 -> -- stream id
  Word32 -> -- raw message id (reply bit intact)
  Int -> -- payload length
  ByteString
pattern ValidHeader csum sid rawId payLen <- (parseHeader -> Just (csum, sid, rawId, payLen))

checkTerminator :: ByteString -> IO ()
checkTerminator bs
  | BS.null bs || BS.head bs /= Terminator = throwIO BadTerminator
  | otherwise = pure ()

verifyChecksum :: ByteString -> ByteString -> Word32 -> IO ()
verifyChecksum hdr body stored = do
  let hdrZeroed = BS.take 4 hdr <> BS.pack [0, 0, 0, 0] <> BS.drop 8 hdr
  let actual = crc32 (hdrZeroed <> body)
  if actual /= stored
    then throwIO ChecksumMismatch
    else pure ()

writePacket :: Handle -> Packet -> IO ()
writePacket h pkt = do
  let wireId = if pkt.isReply then pkt.messageId .|. ReplyBit else pkt.messageId
  let payLen = fromIntegral (BS.length pkt.payload) :: Word32
  let hdr =
        BS.pack $
          w32be Magic
            <> [0, 0, 0, 0]
            <> w32be pkt.stream
            <> w32be wireId
            <> w32be payLen
  let checksum = crc32 (hdr <> pkt.payload)
  let hdrFinal =
        BS.take 4 hdr
          <> BS.pack (w32be checksum)
          <> BS.drop 8 hdr
  BS.hPut h hdrFinal
  BS.hPut h pkt.payload
  BS.hPut h (BS.singleton Terminator)
  hFlush h

readPacket :: Handle -> IO Packet
readPacket h = do
  hdr <- BS.hGet h packetHeaderSize
  case hdr of
    ValidHeader storedCsum sid rawId payLen -> do
      body <- BS.hGet h payLen
      checkTerminator =<< BS.hGet h 1
      verifyChecksum hdr body storedCsum
      let isRep = rawId .&. ReplyBit /= 0
      let msgId = rawId .&. complement ReplyBit
      pure Packet {stream = sid, messageId = msgId, isReply = isRep, payload = body}
    _ -> throwIO (BadMagic (beWord32 hdr 0))
