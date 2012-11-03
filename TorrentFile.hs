module TorrentFile 
	( TorrentFile(..)
	, readTorrent
	, hashByteString
	) where

import Control.Exception
import Control.Monad
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Digest.SHA1
import Data.Map hiding (map)
import Data.Maybe
import Data.Word
import Prelude hiding (readFile, lookup)

import BEncode

{-
BDict (fromList [
	("announce",BString (Chunk "http://tracker.thepiratebay.org/announce" Empty)),
	("announce-list",BList [
		BList [BString (Chunk "http://tracker.thepiratebay.org/announce" Empty)],
		BList [BString (Chunk "udp://tracker.openbittorrent.com:80" Empty)],
		BList [BString (Chunk "udp://tracker.ccc.de:80" Empty)],
		BList [BString (Chunk "udp://tracker.publicbt.com:80" Empty)],
		BList [BString (Chunk "http://tracker.ipv6tracker.org/announce" Empty)],
		BList [BString (Chunk "http://ipv6.tracker.harryy.us/announce" Empty)],
		BList [BString (Chunk "http://ipv4.tracker.harryy.us/announce" Empty)],
		BList [BString (Chunk "udp://tracker.ipv6tracker.org:80/announce" Empty)],
		BList [BString (Chunk "udp://ipv6.tracker.harryy.us:80/announce" Empty)],
		BList [BString (Chunk "udp://ipv4.tracker.harryy.us:80/announce" Empty)],
		BList [BString (Chunk "http://nemesis.1337x.org/announce" Empty)],
		BList [BString (Chunk "http://genesis.1337x.org:1337/announce" Empty)]]),
	("comment",BString (Chunk "Torrent downloaded from http://thepiratebay.se" Empty)),
	("created by",BString (Chunk "mktorrent 1.0" Empty)),
	("creation date",BInt 1346525286),
	("info",BDict (fromList [
		("length",BInt 702260),
		("name",BString (Chunk "Bruce Hood - The Self Illusion: How the Social Brain Creates Identity.mobi" Empty)),
		("piece length",BInt 262144),
		("pieces",BString (Chunk "L\137\161K\181\152\217\183\CANC(s\163Y\\\242\DC4\177\191\191\203u\206\EM\146p{bZ\255e\STX%\SYNK\250vA\EM\214$\225%\199\253\128\NULG\213\241\161\138>\219\nN\"\ACK\138\182" Empty))]))])
-}

data TorrentFile = TorrentFile {
	trackers :: [ByteString],
	name :: ByteString,
	length :: Word64,
	pieceLength :: Word64,
	pieces :: ByteString,
	infoHash :: Word160
} deriving Show

-- stolen from http://searchco.de/codesearch/view/14988216
-- | Convert a Word160 into a ByteString.
hashByteString :: Word160 -> ByteString
hashByteString hash =
		B.pack $ concat $ map toBytes $ toWords hash
	where
		toWords (Word160 a b c d e) = a : b : c : d : e : []
		toBytes a = (w (s a 24)) : (w (s a 16)) : (w (s a 8)) : (w a) : []
			where w = fromIntegral; s = shiftR

readTorrent :: FilePath -> IO TorrentFile
readTorrent fn = do
	f <- B.readFile fn
	let maybeData = bRead f
	when (isNothing maybeData) $ throw $ userError "the file is not bencoded"
	let dict = bDict $ fromJust maybeData
	let al = (map bString . bList) =<< (bList $ fromJust $ lookup "announce-list" dict)
	let binfo = fromJust $ lookup "info" dict
	let ih = hash $ B.unpack $ bPack binfo
	let info = bDict $ binfo
	let len = fromIntegral $ bInt $ fromJust $ lookup "length" info
	let name = bString $ fromJust $ lookup "name" info
	let plength = fromIntegral $ bInt $ fromJust $ lookup "piece length" info
	let pieces = bString $ fromJust $ lookup "pieces" info
	return $ TorrentFile {
		trackers = al,
		TorrentFile.length = len,
		name = name,
		pieceLength = plength,
		pieces = pieces,
		infoHash = ih}
