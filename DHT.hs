{-# LANGUAGE OverloadedStrings #-}

module DHT
	(
	) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.EmbedIO
import Control.Monad.Trans
import Control.Monad.Reader.Class
import qualified Data.Map as M
import Data.Maybe
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC8
import Data.Serialize.Builder
import Network.BSD
import Network.Socket
import Prelude hiding (log)
import System.IO

import Types
import State
import BEncode
import CompactEncoding

--dHTWorker :: NPT ()
--dHTWorker = do
--	s <- ask

getKRPCMessage :: ByteString -> NPT ()
getKRPCMessage b = do
	s <- ask
	qdb <- liftIO $ atomically $ readTVar $ queryDB s
	let KRPC tid pld = decodeKRPCMessage b qdb
	case pld of
		PQuery nid q -> do
			return ()
		PResponse nid r -> do
			return ()
		PError code msg -> log msg

decodeKRPCMessage :: ByteString -> QueryDB -> KRPCMessage
decodeKRPCMessage b qdb = do
	let	md = bRead b
		d = bDict $ fromJust md
		tid = bString $ fromJust $ M.lookup "t" d
		typ = bString $ fromJust $ M.lookup "y" d
	KRPC tid $ case typ of
		"q" ->
			let	q = bString $ fromJust $ M.lookup "q" d
				a = bDict $ fromJust $ M.lookup "a" d
				sid = bString $ fromJust $ M.lookup "id" a in
			PQuery sid $ case q of
				"ping" ->
					QPing
				"find_node" ->
					let	target = bString $ fromJust $ M.lookup "target" a in
					QFindNode target
				"get_peers" ->
					let	ih = bString $ fromJust $ M.lookup "info_hash" a in
					QGetPeers ih
				"announce_peer" ->
					let	ih = bString $ fromJust $ M.lookup "info_hash" a
						port = toEnum $ fromEnum $ bInt $ fromJust $ M.lookup "port" a
						token = bString $ fromJust $ M.lookup "token" a in
					QAnnouncePeer ih port token
		"r" ->
			let	r = bDict $ fromJust $ M.lookup "r" d
				sid = bString $ fromJust $ M.lookup "id" r
				t = fromJust $ M.lookup tid qdb in
			PResponse sid $ case t of
				TPing -> RPing
				TFindNode ->
					let	nodes = getCNI $ bString $ fromJust $ M.lookup "nodes" r in
					RFindNode nodes
				TGetPeers ->
					let	mv = M.lookup "values" r
						token = bString $ fromJust $ M.lookup "token" r in
					case mv of
						Just v ->
							let	peers = map ((\(Right nya) -> nya) . runGetLazy getCompactPeer4 . bString) $ bList v in
							RGetPeers (Right peers) token
						Nothing ->
							let	nodes = getCNI $ bString $ fromJust $ M.lookup "nodes" r in
							RGetPeers (Left nodes) token
				TAnnouncePeer -> RAnnouncePeer
		"e" -> 
			let e_ = bList $ fromJust $ M.lookup "e" d in
			PError (bInt $ e_!!0) (bString $ e_!!1)

encodeKRPCMessage :: KRPCMessage -> ByteString
encodeKRPCMessage (KRPC tid pld) = bPack $ BDict $ M.fromList $
	("t", BString tid) : case pld of
		PQuery nid q -> ("y", BString "q") : case q of
			QPing -> [			("q", BString "ping"),
							("a", BDict $ M.fromList $ [
								("id", BString nid)])]
			QFindNode target -> [		("q", BString "find_node"), 
							("a", BDict $ M.fromList $ [
								("id", BString nid), 
								("target", BString target)])]
			QGetPeers ih -> [		("q", BString "find_node"), 
							("a", BDict $ M.fromList $ [
								("id", BString nid),
								("info_hash", BString ih)])]
			QAnnouncePeer ih port tok -> [	("q", BString "find_node"),
							("a", BDict $ M.fromList $ [
								("id", BString nid),
								("info_hash", BString ih),
								("port", BInt $ fromIntegral $ fromEnum port),
								("token", BString tok)])]
		PResponse nid r -> ("y", BString "r") : case r of
			RPing -> [("r", BDict $ M.fromList $ [
								("id", BString nid)])]
			RFindNode nodes -> [("r", BDict $ M.fromList $ [
								("id", BString nid),
								("nodes", BString $ putList nodes putCompactNodeInfo)])]
			RGetPeers res token -> [("r", BDict $ M.fromList $ [
								("id", BString nid),
								("token", BString token),
								case res of
									Left nodes -> ("nodes", BString $ putList nodes putCompactNodeInfo)
									Right peers -> ("values", BList $ map (BString . toLazyByteString . putCompactPeer) peers)
								])]
			RAnnouncePeer -> [("r", BDict $ M.fromList $ [
								("id", BString nid)])]
		PError code string -> [("y", BString "e"), ("e", BList [BInt code, BString string])]
