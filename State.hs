{-# LANGUAGE TemplateHaskell #-}

module State
	( NPT
	, NPTState(..)
	, runNPT
	, emptyState
	) where

import Control.Concurrent.Chan
import Control.Concurrent.MState
import Control.Monad.State.Class
import Control.Monad.Trans
--import Data.Accessor
--import Data.Accessor.Template
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Prelude hiding (log)

type NPT a = MState NPTState IO a

data NPTState = NPTState {
	log :: ByteString -> IO ()
}

emptyState = NPTState {
	log = const $ return ()
}

-- $( deriveAccessors ''NPTState )

runNPT :: NPT () -> NPTState -> IO ()
runNPT x s = execMState x s >> return ()

--log :: ByteString -> NPT ()
--log s = do
--	--get >>= lift $ writeChan s
--	a <- get
--	return ()
