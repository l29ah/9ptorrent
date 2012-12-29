module TorrentFile 
	( TorrentFile(..)
	, readTorrent
	) where

import Control.Exception
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.List (intercalate)
import Data.Map hiding (map)
import Data.Maybe
import Data.String.Class
import Data.Word
import Prelude hiding (readFile, lookup)

import BEncode
import Types

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

readTorrent :: FilePath -> IO TorrentFile
readTorrent fn = do
	f <- B.readFile fn
	let maybeData = bRead f
	when (isNothing maybeData) $ throw $ userError "the file is not bencoded"
	let dict = bDict $ fromJust maybeData
	let mal = lookup "announce-list" dict
	let al = if isJust mal
		then (map bString . bList) =<< (bList $ fromJust mal)
		else [bString $ fromJust $ lookup "announce" dict]
	let binfo = fromJust $ lookup "info" dict
	let ih = sha1 $ bPack binfo
	let info = bDict $ binfo
	let fl = case lookup "length" info of
		Just len -> Left $ fromIntegral $ bInt $ len
		Nothing -> Right $ map (\d -> let dl x = fromJust $ lookup x $ bDict d in
						(fromIntegral $ bInt $ dl "length", intercalate "/" $ map (toString . bString) $ bList $ dl "path")) $ bList $ fromJust $ lookup "files" info
	let mlen = fromIntegral $ bInt $ fromJust $ lookup "length" info
	let name = bString $ fromJust $ lookup "name" info
	let plength = fromIntegral $ bInt $ fromJust $ lookup "piece length" info
	let pieces = bString $ fromJust $ lookup "pieces" info
	return $ TorrentFile {
		trackers = al,
		filesLength = fl,
		name = name,
		pieceLength = plength,
		pieces = pieces,
		infoHash = ih}
