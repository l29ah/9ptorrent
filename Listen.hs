module Listen
	( listenWire
	, listenDHT
	) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.EmbedIO
import Control.Monad.Trans
import Control.Monad.Reader.Class
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BC8
import Network.BSD
import Network.Socket
import Prelude hiding (log)
import System.IO

import State
import Wire
import DHT

listen' :: HostName -> PortNumber -> ProtocolNumber -> IO Socket
listen' hostname port proto = do
	bracketOnError (socket AF_INET Stream proto) sClose (\sock -> do
		setSocketOption sock ReuseAddr 1
		he <- getHostByName hostname
		bindSocket sock (SockAddrInet port (hostAddress he))
		listen sock maxListenQueue
		return sock)

listenWire :: NPT ()
listenWire = do
	st <- ask
	la <- liftIO $ atomically $ readTVar $ listeningAddr st
	lp <- liftIO $ atomically $ readTVar $ listeningPort st
	proto <- liftIO $ getProtocolNumber "tcp"
	s <- liftIO $ listen' la lp proto
	-- TODO limit the maximum number of peers
	forever $ (liftIO $ accept s) >>= (
		\(s, _) -> forkE . doClient =<< (liftIO $ socketToHandle s ReadWriteMode))

doClient :: Handle -> NPT ()
doClient h = do
	liftIO $ hSetBuffering h NoBuffering
	s <- ask
	tm <- liftIO $ atomically $ readTVar $ torrents s
	rhr <- liftIO $ receiveHandshake h (peerID s) (flip M.member tm)
	case rhr of
		Left error -> log $ "Failed to receive a handshake: " ++ error
		Right (caps, pid, ih) -> undefined
	--chan <- (newChan :: IO (Chan Msg))
	--st <- forkIO $ sender (readChan chan) (BS.hPut h . BS.concat . B.toChunks) -- make a strict bytestring
	--receiver cfg h (writeChan chan)
	--killThread st
	liftIO $ hClose h

listenDHT :: NPT ()
listenDHT = do
	st <- ask
	a <- liftIO $ atomically $ readTVar $ listeningAddr st
	p <- liftIO $ atomically $ readTVar $ listeningPortDHT st
	proto <- liftIO $ getProtocolNumber "udp"
	s <- liftIO $ listen' a p proto
	forever $ (liftIO $ accept s) >>= (
		\(s, _) -> forkE . doDHTClient =<< (liftIO $ socketToHandle s ReadWriteMode))


doDHTClient :: Handle -> NPT ()
doDHTClient h = do
	liftIO $ hSetBuffering h NoBuffering
	s <- ask
	tm <- liftIO $ atomically $ readTVar $ torrents s
	liftIO $ hClose h
