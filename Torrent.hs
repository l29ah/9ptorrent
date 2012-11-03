{-# LANGUAGE OverloadedStrings #-}

module Torrent 
	( Torrent(..)
	, mkTorrent
	) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Word
import Prelude
import System.Random

import TorrentFile
--import Tracker

data Torrent = Torrent {
	torrentFile :: TorrentFile,
	peerID :: ByteString
}

mkTorrent :: FilePath -> IO Torrent
mkTorrent fn = do
	t <- readTorrent fn
	pid <- genPeerID
	return Torrent {
		torrentFile = t,
		peerID = pid}

genPeerID :: IO ByteString
genPeerID = do
	g <- getStdGen
	--let r = (take 20 $ randoms g) :: [Word8]
	return $ B.concat ["-9P0000-", B.pack (take 12 $ randoms g)]
