module Main where

import Control.Concurrent
import Control.Monad.EmbedIO
import Data.ByteString.Lazy (ByteString)
import Network.Curl

import FS
import Listen
import State
import Types
import TorrentFile
import Tracker
import DHT

main = withCurlDo $ do
	lc <- newChan :: IO (Chan ByteString)
	s <- emptyState lc
	runNPT (do
		forkE listenWire
		forkE listenDHT
		runFS lc
	--t <- mkTorrent "Bruce Hood - The Self Illusion: How the Social Brain Creates Identity.mobi.8d076d21e33a449f.torrent"
	--pokeHTTPTracker t
		) s
