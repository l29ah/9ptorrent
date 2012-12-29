{-# LANGUAGE OverloadedStrings #-}

module Types
	( module Data.Word
	, module Control.Concurrent.STM.TVar
	, module Control.Monad
	, Address
	, Host(..)
	, BlockSize
	, Block(..)
	, Torrent(..)
	, TorrentFile(..)
	--, mkTorrent
	, PieceNum
	, InfoHash
	, PeerID
	, genPeerID
	, Peer(..)
	, sha1
	, DHTNodeID
	, genDHTNodeID
	, QueryDB(..)
	, QDBItem(..)
	, KRPCTransactionID(..)
	, KRPCMessage(..)
	, DPayload(..)
	, DQuery(..)
	, DResponse(..)
	) where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC8
import Data.Digest.SHA1
import Data.Map (Map)
import Data.Word
import Network
import Network.Socket.Internal
import System.Random

type Address = (Host, PortNumber)
data Host = StringName HostName | IP4 HostAddress | IP6 HostAddress6 deriving Show

type BlockSize = Int

data Block = Block { blockOffset :: Int        -- ^ offset of this block within the piece
                   , blockSize   :: BlockSize  -- ^ size of this block within the piece
                   } deriving (Eq, Ord, Show)

type PieceNum = Int

type InfoHash = ByteString
type DHTNodeID = InfoHash
type PieceHash = InfoHash

sha1 :: ByteString -> InfoHash
sha1 s = hashByteString $ hash $ B.unpack s

-- stolen from http://searchco.de/codesearch/view/14988216
-- | Convert a Word160 into a ByteString.
hashByteString :: Word160 -> ByteString
hashByteString hash =
		B.pack $ concat $ map toBytes $ toWords hash
	where
		toWords (Word160 a b c d e) = a : b : c : d : e : []
		toBytes a = (w (s a 24)) : (w (s a 16)) : (w (s a 8)) : (w a) : []
			where w = fromIntegral; s = shiftR

type PeerID = ByteString

genPeerID :: IO PeerID
genPeerID = do
	g <- getStdGen
	--let r = (take 20 $ randoms g) :: [Word8]
	return $ B.concat ["-9P0000-", B.pack (take 12 $ randoms g)]

genDHTNodeID :: IO DHTNodeID
genDHTNodeID = do
	g <- getStdGen
	--let r = (take 20 $ randoms g) :: [Word8]
	return $ B.pack (take 20 $ randoms g)

type Pieces = ByteString

data TorrentFile = TorrentFile {
	trackers :: [ByteString],
	name :: ByteString,
	filesLength :: Either Word64 [(Word64, FilePath)],
	pieceLength :: Word64,
	pieces :: Pieces,
	infoHash :: InfoHash
} deriving Show

-- unsafe
getPieceHash :: TorrentFile -> PieceNum -> PieceHash
getPieceHash t n = B.take 20 $ B.drop (fromIntegral $ n * 20) $ pieces t

checkPieceHash :: TorrentFile -> PieceNum -> ByteString -> Bool
checkPieceHash t n s = sha1 s == getPieceHash t n

data Peer = Peer {
	--id :: PeerID,
	address :: Address
} deriving Show

data Torrent = Torrent {
	torrentFile :: TorrentFile,
	knownPeers :: TVar [Peer],
	trackerPokerThread :: ThreadId
} --deriving Show

type KRPCTransactionID = ByteString
type KRPCAnnToken = ByteString

data KRPCMessage = KRPC {
	ktid :: KRPCTransactionID,
	kpld :: DPayload
} deriving Show
data DPayload = PQuery DHTNodeID DQuery | PResponse DHTNodeID QDBItem DResponse | PError Integer ByteString deriving Show
data DQuery = QPing | QFindNode DHTNodeID | QGetPeers InfoHash | QAnnouncePeer InfoHash PortNumber KRPCAnnToken deriving Show
data DResponse = RPing | RFindNode [(DHTNodeID, Address)] | RGetPeers (Either [(DHTNodeID, Address)] [Address]) KRPCAnnToken | RAnnouncePeer deriving Show

type QueryDB = Map KRPCTransactionID QDBItem
data QDBItem = TPing | TFindNode | TGetPeers InfoHash | TAnnouncePeer deriving Show
