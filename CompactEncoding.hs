module CompactEncoding
	( module Data.Serialize.Get
	, getList
	, putList
	, getCompactNodeInfo
	, putCompactNodeInfo
	, getCompactPeer4
	, getCompactPeer6
	, putCompactPeer
	, getCNI
	) where

import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Serialize.Builder
import Data.Serialize.Get
import Numeric

import Types

getList :: ByteString -> Get a -> [a]
getList str g = case runGetLazyState g str of
	Left s -> []
	Right (p, rem) -> p : getList rem g

putList :: [a] -> (a -> Builder) -> ByteString
putList l p = toLazyByteString $ mconcat $ map p l

getCNI :: ByteString -> [(DHTNodeID, Address)]
getCNI b = getList b getCompactNodeInfo

getCompactNodeInfo :: Get (DHTNodeID, Address)
getCompactNodeInfo = do
	id <- getLazyByteString 20
	a <- getCompactPeer4
	return (id, a)

putCompactNodeInfo :: (DHTNodeID, Address) -> Builder
putCompactNodeInfo (nid, a) = mconcat [fromLazyByteString nid, putCompactPeer a]

getCompactPeer4 :: Get Address
getCompactPeer4 = do
	--i1 <- getWord8
	--i2 <- getWord8
	--i3 <- getWord8
	--i4 <- getWord8
	i <- getWord32host
	p <- getWord16host
	--return $ ((show i1 ++ "." ++ show i2 ++ "." ++ show i3 ++ "." ++ show i4), toEnum $ fromEnum p)
	return $ (IP4 i, fromIntegral p)

putCompactPeer :: Address -> Builder
putCompactPeer (hn, p) = mconcat [putCompactHost hn, putWord16host $ fromIntegral p]

putCompactHost :: Host -> Builder
putCompactHost (StringName n) = undefined
putCompactHost (IP4 ip) = putWord32host ip
putCompactHost (IP6 (i1, i2, i3, i4)) = undefined

getCompactPeer6 :: Get Address
getCompactPeer6 = do
	--let s = showHex
	--i1 <- getWord16be
	--i2 <- getWord16be
	--i3 <- getWord16be
	--i4 <- getWord16be
	--i5 <- getWord16be
	--i6 <- getWord16be
	--i7 <- getWord16be
	--i8 <- getWord16be
	i1 <- getWord32be
	i2 <- getWord32be
	i3 <- getWord32be
	i4 <- getWord32be
	p <- getWord16host
	--return $ ((
	--		s i1 ":" ++
	--		s i2 ":" ++
	--		s i3 ":" ++
	--		s i4 ":" ++
	--		s i5 ":" ++
	--		s i6 ":" ++
	--		s i7 ":" ++
	--		s i8 []), toEnum $ fromEnum p)
	return (IP6 (i1, i2, i3, i4), fromIntegral p)
