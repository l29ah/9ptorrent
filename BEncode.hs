module BEncode
	( module Data.BEncode
	, bInt
	, bString
	, bList
	, bDict
	) where

import Data.BEncode
import Data.ByteString.Lazy
import Data.Map

-- Some accessors for the sake of simplicity
bInt :: BEncode -> Integer
bInt (BInt x) = x

bString :: BEncode -> ByteString
bString (BString x) = x

bList :: BEncode -> [BEncode]
bList (BList x) = x

bDict :: BEncode -> Map String BEncode
bDict (BDict x) = x

--bLookup :: String -> BEncode -> BEncode
--bLookup key dict = fromJust $ lookup key $ bDict dict
