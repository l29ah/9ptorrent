module FS (runFS) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8  as B
import Data.Word
import Network.NineP
import Network.NineP.File
import System.Environment
import System.IO

import Torrent
import TorrentFile
import Tracker

configDir = boringDir "config" []
torrentsDir = boringDir "torrents" []

addTorrentFile :: IO NineFile
addTorrentFile = do
	c <- newChan :: IO (Chan ByteString)
	forkIO $ forever $ handle (\e -> do	let err = show (e :: IOException)
						hPutStr stderr ("Couldn't start torrent: " ++ err)
						return ()) $ do
		fn <- readChan c
		t <- mkTorrent $ B.unpack fn
		pokeHTTPTracker t
	return $ chanFile "add_torrent" Nothing (Just c)

runFS = do
	a <- getEnv "NPTORRENT_ADDRESS"
	atf <- addTorrentFile
	run9PServer $ Config {
		root = boringDir "/" [
			("config", configDir),
			("torrents", torrentsDir),
			("add_torrent", atf)
		],
		addr = a}
