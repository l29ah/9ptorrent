module Torrent where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC8
import qualified Data.Map as M

import State
import TorrentFile
import Tracker
import Types

mkTorrent :: FilePath -> IO Torrent
mkTorrent fn = do
	t <- readTorrent fn
	kp <- newTVarIO []
	--pid <- genPeerID
	return Torrent {
		torrentFile = t,
		knownPeers = kp,
		trackerPokerThread = undefined
	}

addTorrent :: ByteString -> NPT ()
addTorrent fn = do
	t_ <- liftIO $ mkTorrent $ BC8.unpack fn
	tp <- forkE $ trackerPoker t_
	s <- ask
	let t = t_ {
		trackerPokerThread = tp
	}
	liftIO $ atomically $ do
		tm <- readTVar $ torrents s
		writeTVar (torrents s) $ M.insert (infoHash $ torrentFile t) t tm

addPeer :: InfoHash -> Address -> NPT ()
addPeer ih addr = undefined

delTorrent :: InfoHash -> NPT ()
delTorrent ih = undefined

trackerPoker :: Torrent -> NPT ()
trackerPoker t = forever $ do
	pokeTrackers t
	liftIO $ threadDelay 600000000
