{-# LANGUAGE OverloadedStrings #-}

module Tracker where

--import Data.Binary.Strict.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Data.Maybe
import Data.Serialize.Get
import Data.Word
import Network.HTTP hiding (Response)	-- HTTP doesn't support IPv6; using only urlencode from it
--import Network.Curl	-- Curl breaks responses at unusual symbols
import Network.Curl.Download
import Numeric

import Torrent
import TorrentFile
import BEncode

type RPeer = (ByteString, ByteString, Word16)

data Response = Response {
	interval :: Word32,
	minInterval :: Word32,	-- defaults to interval
	-- complete
	-- downloaded
	-- incomplete
	peers :: [RPeer]
	} deriving Show

listenPort = 6881

httpTrackers :: [ByteString] -> [ByteString]
httpTrackers l = filter (B.isPrefixOf "http://") l

udpTrackers :: [ByteString] -> [ByteString]
udpTrackers l = filter (B.isPrefixOf "udp://") l

pokeHTTPTracker :: Torrent -> IO ()
pokeHTTPTracker torrent = do
	let tf = torrentFile torrent
	-- Got through http://extratorrent.com/article/71/public+open+torrent+trackers.html
	let t = B.unpack $ (httpTrackers $ trackers tf) !! 1
	--let t = "http://tracker.istole.it:80/announce"
	--let t = "http://bittrk.appspot.com/announce"	-- violates bencoding standard
	--let t = "http://tracker.torrentbox.com:2710/announce"	-- requires auth
	--let t = "http://www.h33t.com:3310/announce"	-- returns packed peers list
	--let t = "http://tracker.openbittorrent.kg:2710/announce"	-- requires auth
	--let t = "http://tracker.torrent.to:2710/announce"	-- returns packed peers list
	--let t = "http://tracker.torrentbox.com:2710/announce"	-- requires auth
	let vars = urlEncodeVars [
		("info_hash", B.unpack $ hashByteString $ infoHash tf),
		("peer_id", B.unpack $ peerID torrent),
		("port", show listenPort),
		("uploaded", "0"),
		("downloaded", "0"),
		("left", "1")]
	let r = concat [t, "?", vars]
	print $ r
	--resp <- simpleHTTP $ getRequest r
	--rb <- getResponseBody resp
	--(_, rb) <- curlGetString r []
	Right rb <- openURI r
	print rb
	-- "d8:completei1e10:downloadedi0e10:incompletei1e8:intervali1904e12:min intervali952e5:peers12:]P\202[\SUB\225_\142\172\198\SUB\re"
	print $ decodeResponse $ B.fromChunks $ rb : []

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
parsePeers (BString d) = go d
	where go str = case runGetLazyState getCompactPeer4 str of
		Left s -> []
		Right (p, rem) -> p : go rem

parsePeers6 :: BEncode -> [RPeer]
parsePeers6 (BDict d) = error "Traditional peer lists are unsupported for IPv6. Send me an example of one plzkthx."
parsePeers6 (BString d) = go d
	where go str = case runGetLazyState getCompactPeer6 str of
		Left s -> []
		Right (p, rem) -> p : go rem
					

getCompactPeer4 :: Get RPeer
getCompactPeer4 = do
	i1 <- getWord8
	i2 <- getWord8
	i3 <- getWord8
	i4 <- getWord8
	p <- getWord16be
	return ("", B.pack $ show i1 ++ "." ++ show i2 ++ "." ++ show i3 ++ "." ++ show i4, p)

getCompactPeer6 :: Get RPeer
getCompactPeer6 = do
	let s = showHex
	i1 <- getWord16be
	i2 <- getWord16be
	i3 <- getWord16be
	i4 <- getWord16be
	i5 <- getWord16be
	i6 <- getWord16be
	i7 <- getWord16be
	i8 <- getWord16be
	p <- getWord16be
	return ("", B.pack $
			s i1 ":" ++
			s i2 ":" ++
			s i3 ":" ++
			s i4 ":" ++
			s i5 ":" ++
			s i6 ":" ++
			s i7 ":" ++
			s i8 [], p)
