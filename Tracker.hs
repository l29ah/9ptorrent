{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Tracker where

import Control.Monad.EmbedIO
--import Data.Binary.Strict.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC8
import qualified Data.Map as M
import Data.Maybe
import Data.Serialize.Get
import Data.String.Class
import Data.Word
import Network.HTTP hiding (Response)	-- HTTP doesn't support IPv6; using only urlencode from it
--import Network.Curl	-- Curl breaks responses at unusual symbols
import Network.Curl.Download
import Network
import Numeric
import Prelude hiding (log, concat)

import State
import Types
import TorrentFile
import BEncode
import CompactEncoding

data RPeer = RPeer {
	id :: ByteString,
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
		let tf = torrentFile torrent
		--let ht = httpTrackers $ trackers tf
		mapM_ (forkE . pokeTracker torrent) $ trackers tf
	
pokeTracker :: Torrent -> ByteString -> NPT ()
pokeTracker t tr = do
	resp <- pokeTracker_ t tr
	log $ append "Response: " $ show resp

pokeTracker_ t tr | B.isPrefixOf "http://" tr = pokeHTTPTracker t tr
--pokeTracker_ tr | B.isPrefixOf "udp://" = pokeUDPTracker tr
pokeTracker_ _ _ = return Nothing



--httpTrackers :: [ByteString] -> [ByteString]
--httpTrackers l = filter (B.isPrefixOf "http://") l
--
--udpTrackers :: [ByteString] -> [ByteString]
--udpTrackers l = filter (B.isPrefixOf "udp://") l

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
parsePeers (BString d) = map (RPeer "") $ getList d getCompactPeer4

parsePeers6 :: BEncode -> [RPeer]
parsePeers6 (BDict d) = error "Traditional peer lists are unsupported for IPv6. Send me an example of one plzkthx."
parsePeers6 (BString d) = map (RPeer "") $ getList d getCompactPeer6
