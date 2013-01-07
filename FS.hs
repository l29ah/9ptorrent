module FS (runFS) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Control.Monad.EmbedIO
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8  as B
import Data.Word
import Network.NineP
import Network.NineP.File
import Network.NineP.File.Instances
import Prelude hiding (log)
import System.Environment
import System.IO

import State
import Types
import Torrent
import TorrentFile
import Tracker

mkDir :: String -> [(String, NPT (NineFile NPT))] -> NPT (NineFile NPT)
--mkDir [] = return []
--mkDir ((n,a):xs) = do
--	return $ (n, f) : mkDir xs
mkDir name contents = do
	let (ns, as) = unzip contents
	fs <- sequence as
	return $ boringDir name $ zip ns fs

tVar v = (atomically $ readTVar v, atomically . writeTVar v)

useRetrackerF :: NPT (NineFile NPT)
useRetrackerF = do
	s <- ask
	return . join (simpleFile "use_retracker") $ useRetracker s

configDir = mkDir "config" [
		("use_retracker", useRetrackerF)
	]
torrentsDir = boringDir "torrents" []

sniffE :: NPT a -> NPT a
sniffE act = do
	r <- tryE act
	either ((\e -> do
		liftIO $ putStrLn $ show e
		log $ show e
		throwE e) :: SomeException -> NPT a) return r

{-
torrentAdder :: Chan ByteString -> NPT ()
torrentAdder c = do
	lift $ forever $ handle (\e -> do
					let err = show (e :: IOException)
					log $ "Couldn't start torrent: " ++ err
					return ()) $ do
		fn <- readChan c
		t <- mkTorrent $ B.unpack fn
		pokeHTTPTracker t


addTorrentFile :: (ByteString -> IO ()) -> NPT (NineFile NPT)
addTorrentFile = do
	c <- lift $ newChan :: NPT (Chan ByteString)
	forkE $ torrentAdder c
	return $ chanFile "add_torrent" Nothing (Just c)
-}
--addTorrentFile :: NPT (NineFile NPT)
addTorrentFile = rwFile "add_torrent" Nothing $ Just $ sniffE . addTorrent

addConfigUseRetracker = rwFile "retracker" Nothing $ Just $ sniffE . addTorrent

addLogFile :: Chan ByteString -> IO (NineFile NPT)
addLogFile c = do
	return $ simpleFile "log" c ()

runFS :: Chan ByteString -> NPT ()
runFS lc = do
	a <- lift $ getEnv "NPTORRENT_ADDRESS"
	-- initialize files
	logf <- lift $ addLogFile lc
	let atf = addTorrentFile
	-- launch the filesystem
	s <- ask
	cd <- configDir
	lift $ run9PServer $ Config {
		root = boringDir "/" [
			("config", cd),
			("torrents", torrentsDir),
			("add_torrent", atf),
			("log", logf)
		],
		addr = a,
		monadState = s
		}
