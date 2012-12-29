{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Tracker where

import Control.Exception
import Control.Monad.EmbedIO
--import Data.Binary.Strict.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC8
import qualified Data.Map as M
import Data.Maybe
import Data.Serialize.Get
import Data.Serialize.Builder as CB hiding (append)
import Data.String.Class as S
import Data.Word
import Data.Monoid
import Network.HTTP hiding (Response)	-- HTTP doesn't support IPv6; using only urlencode from it
--import Network.Curl	-- Curl breaks responses at unusual symbols
import Network.Curl.Download
import Network
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString.Lazy
import Numeric
import Prelude as P hiding (log, concat)
import System.Random

import State
import Types
import TorrentFile
import BEncode
import CompactEncoding

data RPeer = RPeer {
	id :: Maybe PeerID,
	address :: Address
} deriving Show

data Response = Response {
	interval :: Word32,
	minInterval :: Word32,	-- defaults to interval
	-- complete
	-- downloaded
	-- incomplete
	peers :: [RPeer]
	} deriving Show

listenPort = 6881

pokeTrackers :: Torrent -> NPT ()
pokeTrackers torrent = do
	s <- ask
	let tf = torrentFile torrent
	urt <- liftIO $ atomically $ readTVar $ useRetracker s
	let tracks = if urt then "retracker.local" : trackers tf else trackers tf
	--let ht = httpTrackers $ trackers tf
	--mapM_ (forkE . pokeTracker torrent) $ trackers tf
	mapM_ (handleE (\(e :: SomeException) -> log $ show e) . pokeTracker torrent) tracks
	
pokeTracker :: Torrent -> ByteString -> NPT ()
pokeTracker t tr = do
	log $ append "Poking tracker: " tr
	resp <- pokeTracker_ t tr
	log $ append "Response: " $ show resp

pokeTracker_ t tr | B.isPrefixOf "http://" tr = pokeHTTPTracker t tr
pokeTracker_ t tr | B.isPrefixOf "udp://" tr = pokeUDPTracker t tr
pokeTracker_ _ _ = return Nothing



--httpTrackers :: [ByteString] -> [ByteString]
--httpTrackers l = filter (B.isPrefixOf "http://") l
--
--udpTrackers :: [ByteString] -> [ByteString]
--udpTrackers l = filter (B.isPrefixOf "udp://") l
{-
Time outs

UDP is an 'unreliable' protocol. This means it doesn't retransmit lost packets itself. The application is responsible for this. If a response is not received after 15 * 2 ^ n seconds, the client should retransmit the request, where n starts at 0 and is increased up to 8 (3840 seconds) after every retransmission. Note that it is necessary to rerequest a connection ID when it has expired.
 -}

type TransactionID = Word32
type ConnectionID = Word64
data Event = ENone | ECompleted | EStarted | EStopped
data TUDPQuery = QConnect TransactionID | QAnnounce ConnectionID TransactionID InfoHash PeerID Word64 Word64 Word64 Event (Maybe HostAddress) Word32 Word32 PortNumber
data TUDPResponse	= RConnect TransactionID ConnectionID
			| RAnnounce {
				ratid :: TransactionID,
				rainterval :: Word32,
				raleechers :: Word32,
				raseeders :: Word32,
				rapeers :: [Address]
			} deriving Show

putEv ENone = putWord32be 0
putEv ECompleted = putWord32be 1
putEv EStarted = putWord32be 2
putEv EStopped = putWord32be 3

putTUDPQuery :: TUDPQuery -> Builder
putTUDPQuery (QConnect tid) = mconcat [
	putWord64be 0x41727101980,
	putWord32be 0,
	putWord32be tid]
putTUDPQuery (QAnnounce cid tid ih pid down left up ev addr key want port) = mconcat [
	putWord64be cid,
	putWord32be 1,
	putWord32be tid,
	CB.fromLazyByteString ih,
	CB.fromLazyByteString pid,
	putWord64be down,
	putWord64be left,
	putWord64be up,
	putEv ev,
	putIP4 $ fromMaybe 0 addr,
	putWord32be key,
	putWord32be want,
	putPort port]

getTUDPResponse :: Get TUDPResponse
getTUDPResponse = do
	action <- getWord32be
	case action of
		0 -> do
			tid <- getWord32be
			cid <- getWord64be
			return $ RConnect tid cid
		1 -> do
			tid <- getWord32be
			interval <- getWord32be
			leechers <- getWord32be
			seeders <- getWord32be
			rem <- remaining
			peers <- sequence $ replicate (rem `div` 6) $ getCompactPeer4
			return $ RAnnounce tid interval leechers seeders peers

pokeUDPTracker :: Torrent -> ByteString -> NPT (Maybe Response)
pokeUDPTracker torrent t_ = do
	s <- ask
	let peerid = peerID s
	let tf = torrentFile torrent
	lp <- liftIO $ atomically $ readTVar $ listeningPort s

	-- presume we got "udp://tracker.publicbt.com:80" format
	let (host, port_) = BC8.break (== ':') $ S.drop 6 t_
	let port = toEnum $ (read $ P.tail $ toString port_ :: Int) :: PortNumber
	sock <- liftIO $ socket AF_INET Datagram 0
	--ha <- liftIO $ inet_addr "127.0.0.1"
	(AddrInfo { addrAddress = SockAddrInet _ addr }):_ <- liftIO $ getAddrInfo (Just $ AddrInfo [] AF_INET Datagram defaultProtocol undefined Nothing) (Just $ toString host) Nothing
	liftIO $ connect sock $ SockAddrInet port addr

	let say = liftIO . send sock . CB.toLazyByteString . putTUDPQuery 
	let hear = liftIO $ liftM ((\(Right nya) -> nya) . runGetLazy getTUDPResponse) $ recv sock 9000 

	t1id <- liftIO randomIO
	say $ QConnect t1id
	RConnect t1id_ cid <- hear
	when (t1id /= t1id_) $ fail "FIXME got some other transaction ID"

	t2id <- liftIO randomIO
	say $ QAnnounce cid t2id (infoHash tf) peerid 0 0 0 ENone Nothing 0 (-1) lp
	ra <- hear
	when (t2id /= ratid ra) $ fail "FIXME got some other transaction ID"
	return $ Just $ Response (rainterval ra) (rainterval ra) $ map (RPeer Nothing) $ rapeers ra

pokeHTTPTracker :: Torrent -> ByteString -> NPT (Maybe Response)
pokeHTTPTracker torrent t_ = do
	s <- ask
	let peerid = peerID s
	-- Got through http://extratorrent.com/article/71/public+open+torrent+trackers.html
	let t = BC8.unpack t_
	--let t = "http://tracker.istole.it:80/announce"
	--let t = "http://bittrk.appspot.com/announce"	-- violates bencoding standard
	--let t = "http://tracker.torrentbox.com:2710/announce"	-- requires auth
	--let t = "http://www.h33t.com:3310/announce"	-- returns packed peers list
	--let t = "http://tracker.openbittorrent.kg:2710/announce"	-- requires auth
	--let t = "http://tracker.torrent.to:2710/announce"	-- returns packed peers list
	--let t = "http://tracker.torrentbox.com:2710/announce"	-- requires auth
	let tf = torrentFile torrent
	let vars :: String = urlEncodeVars [
			("info_hash", BC8.unpack $ infoHash tf),
			("peer_id", BC8.unpack $ peerid),
			("port", show listenPort),
			("uploaded", "0"),
			("downloaded", "0"),
			("left", "1")]
	let r = concat [t, "?", vars]
	log $ show $ r
	--resp <- simpleHTTP $ getRequest r
	--rb <- getResponseBody resp
	--(_, rb) <- curlGetString r []
	rb_ <- liftIO $ openURI r
	case rb_ of
		Left e -> log $ show e
		_ -> return ()
	let Right rb = rb_
	log $ append "Answer from HTTP: " rb
	-- "d8:completei1e10:downloadedi0e10:incompletei1e8:intervali1904e12:min intervali952e5:peers12:]P\202[\SUB\225_\142\172\198\SUB\re"
	let resp = decodeResponse $ B.fromChunks $ rb : []
	case resp of
		Left e -> do
			log $ show e
			return Nothing
		Right r -> do
			liftIO $ print r
			return $ Just r

decodeResponse :: ByteString -> Either ByteString Response
decodeResponse b = let	md = bRead b
			d = bDict $ fromJust md
			fail = M.lookup "failure reason" d in
	if isNothing md 
		then Left "can't decode bencoded tracker answer"
		else if isJust fail
			then Left $ bString $ fromJust fail
			else Right Response {
					interval = fromIntegral $ bInt $ fromJust $ M.lookup "interval" d,
					minInterval = maybe 0 (fromIntegral . bInt) $ M.lookup "min interval" d,
					peers = parsePeers_ d }

parsePeers_ :: M.Map String BEncode -> [RPeer]
parsePeers_ d = let	mp4 = M.lookup "peers" d
			mp6 = M.lookup "peers6" d in
		maybe [] (parsePeers) mp4 ++ maybe [] (parsePeers6) mp6


parsePeers :: BEncode -> [RPeer]
parsePeers (BDict d) = error "Traditional peer lists are unsupported. Send me an example of one plzkthx."
parsePeers (BString d) = map (RPeer Nothing) $ getList d getCompactPeer4

parsePeers6 :: BEncode -> [RPeer]
parsePeers6 (BDict d) = error "Traditional peer lists are unsupported for IPv6. Send me an example of one plzkthx."
parsePeers6 (BString d) = map (RPeer Nothing) $ getList d getCompactPeer6
