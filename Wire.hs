{-# LANGUAGE OverloadedStrings #-}

module Wire
--    ( Message(..)
--    , encodePacket
--    , decodeMsg
--    , constructBitField
--    -- Handshaking
--    , initiateHandshake
--    , receiveHandshake
--    -- Tests
--    , testSuite
--    )
where

import Control.Applicative hiding (empty)
import Control.Monad
--import Control.DeepSeq

import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Lazy as L

import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get

import Data.Char
import Network.Socket (PortNumber)
import System.IO
--import System.Log.Logger

--import Test.Framework
--import Test.Framework.Providers.QuickCheck2
--import Test.QuickCheck
--
--import Torrent

import Types

debugM :: String -> String -> IO ()
debugM _ _ = return ()

type BitField    = L.ByteString
type PieceLength = Int

data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have PieceNum -- Int
             | BitField BitField
             | Request PieceNum Block
             | Piece PieceNum Int B.ByteString
             | Cancel PieceNum Block
             | Port PortNumber
  deriving (Eq, Show)

--instance NFData Message where
--    rnf a = a `seq` ()

--instance Arbitrary Message where
--    arbitrary = oneof [return KeepAlive, return Choke, return Unchoke, return Interested,
--                       return NotInterested,
--                       Have <$> arbitrary,
--                       BitField <$> arbitrary,
--                       Request <$> arbitrary <*> arbitrary,
--                       Piece <$> arbitrary <*> arbitrary <*> arbitrary,
--                       Cancel <$> arbitrary <*> arbitrary,
--                       Port <$> choose (0,16383)]
--

data HandShake = HandShake 
                  String -- Protocol Header
                  Word64 -- Extension Bias

-- | The Protocol header for the Peer Wire Protocol
protocolHeader :: B.ByteString
protocolHeader = "BitTorrent protocol"

extensionBasis :: Word64
extensionBasis = 0

extensionFast :: Word64
extensionFast = 4

p8 :: Word8 -> Put
p8 = putWord8

p32be :: Integral a => a -> Put
p32be = putWord32be . fromIntegral

decodeMsg :: Get Message
decodeMsg = {-# SCC "decodeMsg" #-} get

encodePacket :: Message -> B.ByteString
encodePacket m = mconcat [szEnc, mEnc]
  where mEnc  = encode m
        sz    = B.length mEnc
        szEnc = runPut . p32be $ sz

instance Serialize Message where
    put KeepAlive       = return ()
    put Choke           = p8 0
    put Unchoke         = p8 1
    put Interested      = p8 2
    put NotInterested   = p8 3
    put (Have pn)       = p8 4 *> p32be pn
    put (BitField bf)   = p8 5 *> putLazyByteString bf
    put (Request pn (Block os sz))
                        = p8 6 *> mapM_ p32be [pn,os,sz]
    put (Piece pn os c) = p8 7 *> mapM_ p32be [pn,os] *> putByteString c
    put (Cancel pn (Block os sz))
                        = p8 8 *> mapM_ p32be [pn,os,sz]
    put (Port p)        = p8 9 *> (putWord16be . fromIntegral $ p)

    get =  getKA      <|> getChoke
       <|> getUnchoke <|> getIntr
       <|> getNI      <|> getHave
       <|> getBF      <|> getReq
       <|> getPiece   <|> getCancel
       <|> getPort

getBF, getChoke, getUnchoke, getIntr, getNI, getHave, getReq :: Get Message
getPiece, getCancel, getPort, getKA :: Get Message
getChoke   = byte 0 *> return Choke
getUnchoke = byte 1 *> return Unchoke
getIntr    = byte 2 *> return Interested
getNI      = byte 3 *> return NotInterested
getHave    = byte 4 *> (Have <$> gw32)
getBF      = byte 5 *> (BitField <$> (remaining >>= getLazyByteString . fromIntegral))
getReq     = byte 6 *> (Request  <$> gw32 <*> (Block <$> gw32 <*> gw32))
getPiece   = byte 7 *> (Piece    <$> gw32 <*> gw32 <*> (remaining >>= getByteString))
getCancel  = byte 8 *> (Cancel   <$> gw32 <*> (Block <$> gw32 <*> gw32))
getPort    = byte 9 *> (Port . fromIntegral <$> getWord16be)
getKA      = do
    empty <- isEmpty
    if empty
        then return KeepAlive
        else fail "Non empty message - not a KeepAlive"

gw32 :: Integral a => Get a
gw32 = fromIntegral <$> getWord32be

byte :: Word8 -> Get Word8
byte w = do
    x <- lookAhead getWord8
    if x == w
        then getWord8
        else fail $ "Expected byte: " ++ show w ++ " got: " ++ show x

-- | Size of the protocol header
protocolHeaderSize :: Int
protocolHeaderSize = B.length protocolHeader

-- | Protocol handshake code. This encodes the protocol handshake part
protocolHandshake :: L.ByteString
protocolHandshake = L.fromChunks [
			runPut $ putWord8 $ fromIntegral protocolHeaderSize,
			protocolHeader,
			runPut $ putWord64be extensionBasis]

toBS :: String -> B.ByteString
toBS = B.pack . map toW8

toLBS :: String -> L.ByteString
toLBS = L.pack . map toW8

fromLBS :: L.ByteString -> String
fromLBS = map (chr . fromIntegral) . L.unpack

toW8 :: Char -> Word8
toW8 = fromIntegral . ord


-- | Receive the header parts from the other end
receiveHeader :: Handle -> Int -> (InfoHash -> Bool)
              -> IO (Either String ([Capabilities], PeerID, InfoHash))
receiveHeader h sz ihTst = parseHeader `fmap` B.hGet h sz
  where parseHeader = runGet (headerParser ihTst)

headerParser :: (InfoHash -> Bool) -> Get ([Capabilities], PeerID, InfoHash)
headerParser ihTst = do
    hdSz <- getWord8
    when (fromIntegral hdSz /= protocolHeaderSize) $ fail "Wrong header size"
    protoString <- getByteString protocolHeaderSize
    when (protoString /= protocolHeader) $ fail "Wrong protocol header"
    caps <- getWord64be
    ihR  <- getLazyByteString 20
    unless (ihTst ihR) $ fail "Unknown InfoHash"
    pid <- getLazyByteString 20
    return (decodeCapabilities caps, pid, ihR)


data Capabilities = Fast
decodeCapabilities :: Word64 -> [Capabilities]
decodeCapabilities _ = []

-- | Initiate a handshake on a socket
initiateHandshake :: Handle -> PeerID -> InfoHash
                  -> IO (Either String ([Capabilities], PeerID, InfoHash))
initiateHandshake handle peerid infohash = do
    debugM "Protocol.Wire" "Sending off handshake message"
    L.hPut handle msg
    hFlush handle
    debugM "Protocol.Wire" "Receiving handshake from other end"
    receiveHeader handle sz (== infohash)
  where msg = handShakeMessage peerid infohash
        sz = fromIntegral (L.length msg)

-- | Construct a default handshake message from a PeerID and an InfoHash
handShakeMessage :: PeerID -> InfoHash -> L.ByteString
handShakeMessage pid ih = L.concat [protocolHandshake, ih, pid]

-- | Receive a handshake on a socket
receiveHandshake :: Handle -> PeerID -> (InfoHash -> Bool)
                 -> IO (Either String ([Capabilities], PeerID, InfoHash))
receiveHandshake h pid ihTst = do
    debugM "Protocol.Wire" "Receiving handshake from other end"
    r <- receiveHeader h sz ihTst
    case r of
        Left err -> return $ Left err
        Right (caps, rpid, ih) ->
            do debugM "Protocol.Wire" "Sending back handshake message"
               L.hPut h (msg ih)
               hFlush h
               return $ Right (caps, rpid, ih)
  where msg ih = handShakeMessage pid ih
        sz = fromIntegral (L.length $ msg "12345678901234567890") -- Dummy value
--
--
---- | The call @constructBitField pieces@ will return the a ByteString suitable for inclusion in a
----   BITFIELD message to a peer.
--constructBitField :: Int -> [PieceNum] -> L.ByteString
--constructBitField sz pieces = L.pack . build $ m
--    where m = map (`elem` pieces) [0..sz-1 + pad]
--          pad = 8 - (sz `mod` 8)
--          build [] = []
--          build l = let (first, rest) = splitAt 8 l
--                    in if length first /= 8
--                       then error "Wront bitfield"
--                       else bytify first : build rest
--          bytify [b7,b6,b5,b4,b3,b2,b1,b0] = sum [if b0 then 1   else 0,
--                                                  if b1 then 2   else 0,
--                                                  if b2 then 4   else 0,
--                                                  if b3 then 8   else 0,
--                                                  if b4 then 16  else 0,
--                                                  if b5 then 32  else 0,
--                                                  if b6 then 64  else 0,
--                                                  if b7 then 128 else 0]
--
--
-- -- TESTS
--testSuite :: Test
--testSuite = testGroup "Protocol/Wire"
--  [ testProperty "QC encode-decode/id" propEncodeDecodeId]
--
--
--propEncodeDecodeId :: Message -> Bool
--propEncodeDecodeId m =
--    let encoded = encode m
--        decoded = decode encoded
--    in
--        Right m == decoded
--
--
