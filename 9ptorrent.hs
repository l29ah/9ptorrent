module Main where

import Control.Concurrent
import Network.Curl

import FS
import State
import Torrent
import TorrentFile
import Tracker

main = withCurlDo $ runNPT (do
		runFS
	--t <- mkTorrent "Bruce Hood - The Self Illusion: How the Social Brain Creates Identity.mobi.8d076d21e33a449f.torrent"
	--pokeHTTPTracker t
	) emptyState
