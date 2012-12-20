{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}

module State
	( module Control.Monad.IO.Class
	, module Control.Monad.Reader.Class
	, module Control.Monad.EmbedIO
	, module Control.Concurrent.STM
	, module Control.Concurrent.STM.TVar
	, NPT
	, NPTState(..)
	, runNPT
	, emptyState
	, log
	) where

import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.EmbedIO
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Trans
--import Data.Accessor
--import Data.Accessor.Template
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC8
import Data.Map (Map)
import qualified Data.Map as M
import Data.String.Class
import Data.Word
import Network.Socket
import Prelude hiding (log, putStrLn)

import Types

type NPT = ReaderT NPTState IO 

data NPTState = NPTState {
	log_ :: ByteString -> IO (),
	listeningAddr :: TVar HostName,
	listeningPort :: TVar PortNumber,
	torrents :: TVar (Map InfoHash Torrent),
	peerID :: PeerID,

	-- DHT-related
	listeningPortDHT :: TVar PortNumber,
	dHTNodeID :: DHTNodeID,
	bootstrapNodes :: [Address],
	queryDB :: TVar QueryDB
}

emptyState :: Chan ByteString -> IO NPTState
emptyState lc = do
	la <- newTVarIO "localhost"
	lp <- newTVarIO 43789
	lpd <- newTVarIO 43788
	tors <- newTVarIO M.empty
	qdb <- newTVarIO M.empty
	pid <- genPeerID
	nid <- genDHTNodeID
	return $ NPTState {
		log_ = writeChan lc,
		listeningAddr = la,
		listeningPort = lp,
		listeningPortDHT = lpd,
		bootstrapNodes = [
			(StringName "dht.transmissionbt.com", 6881),
			(StringName "router.bittorrent.com", 6881)
		],
		peerID = pid,
		dHTNodeID = nid,
		torrents = tors,
		queryDB = qdb
	}

-- $( deriveAccessors ''NPTState )

--log :: ConvLazyByteString a => a -> NPT ()
--log = log__ . toLazyByteString
log :: StringRWIO s => s -> NPT ()
log = liftIO . putStrLn

log__ :: ByteString -> NPT ()
log__ nya = do
	a <- ask
	liftIO $ log_ a $ BC8.snoc nya '\n'

runNPT_ :: NPT a -> NPTState -> IO a
runNPT_ x s = do
	runReaderT x s

runNPT :: NPT () -> NPTState -> IO ()
runNPT x s = do
	runNPT_ x s
	return ()

--log :: ByteString -> NPT ()
--log s = do
--	--get >>= lift $ writeChan s
--	a <- get
--	return ()

instance EmbedIO NPT where
	type Content NPT = NPTState
	embed f = do
		s <- ask
		liftIO $ f s
	callback act cont = runNPT_ act cont
