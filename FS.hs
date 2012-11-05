module FS (runFS) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MState
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8  as B
import Data.Word
import Network.NineP
import Network.NineP.File
import Prelude hiding (log)
import System.Environment
import System.IO

import State
import Torrent
import TorrentFile
import Tracker

configDir = boringDir "config" []
torrentsDir = boringDir "torrents" []

torrentAdder :: Chan ByteString -> NPT ()
torrentAdder c = do
	s <- get
	let l = log s
	lift $ forever $ handle (\e -> do
					let err = show (e :: IOException)
					l $ B.pack $ "Couldn't start torrent: " ++ err
					return ()) $ do
		fn <- readChan c
		t <- mkTorrent $ B.unpack fn
		pokeHTTPTracker t


addTorrentFile :: (ByteString -> IO ()) -> NPT NineFile
addTorrentFile say = do
	c <- lift $ newChan :: NPT (Chan ByteString)
	forkM $ torrentAdder c
	return $ chanFile "add_torrent" Nothing (Just c)

addLogFile :: IO (ByteString -> IO (), NineFile)
addLogFile = do
	c <- newChan :: IO (Chan ByteString)
	return $ (writeChan c, chanFile "log" (Just c) Nothing)

runFS :: NPT ()
runFS = do
	a <- lift $ getEnv "NPTORRENT_ADDRESS"
	(say, logf) <- lift $ addLogFile
	atf <- addTorrentFile say
	lift $ run9PServer $ Config {
		root = boringDir "/" [
			("config", configDir),
			("torrents", torrentsDir),
			("add_torrent", atf),
			("log", logf)
		],
		addr = a}
