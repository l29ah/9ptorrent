{-# LANGUAGE OverloadedStrings #-}

module Types
	( module Data.Word
	, module Control.Concurrent.STM.TVar
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
	, sha1
	, DHTNodeID
	, genDHTNodeID
	, QueryDB(..)
	, QDBItem(..)
	, TransactionID(..)
	, KRPCMessage(..)
	, DPayload(..)
	, DQuery(..)
	, DResponse(..)
	) where

import Control.Concurrent
import Control.Concurrent.STM.TVar
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

data TorrentFile = TorrentFile {
	trackers :: [ByteString],
	name :: ByteString,
	length :: Word64,
	pieceLength :: Word64,
	pieces :: ByteString,
	infoHash :: InfoHash
} deriving Show

data Peer = Peer {
	id :: ByteString,
	address :: Address
} deriving Show

data Torrent = Torrent {
	torrentFile :: TorrentFile,
	knownPeers :: TVar [Peer],
	trackerPokerThread :: ThreadId
} --deriving Show

type TransactionID = ByteString
type AnnToken = ByteString

data KRPCMessage = KRPC {
	ktid :: TransactionID,
	kpld :: DPayload
} deriving Show
data DPayload = PQuery DHTNodeID DQuery | PResponse DHTNodeID DResponse | PError Integer ByteString deriving Show
data DQuery = QPing | QFindNode DHTNodeID | QGetPeers InfoHash | QAnnouncePeer InfoHash PortNumber AnnToken deriving Show
data DResponse = RPing | RFindNode [(DHTNodeID, Address)] | RGetPeers (Either [(DHTNodeID, Address)] [Address]) AnnToken | RAnnouncePeer deriving Show

type QueryDB = Map TransactionID QDBItem
data QDBItem = TPing | TFindNode | TGetPeers | TAnnouncePeer deriving Show
