{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.ByteString.Char8 as B
import Data.Binary.Get as G
import Data.Binary.Put as P
import ZMQHS.Frame (parse)

test_data = do
	return B.pack "\000\000\000"

main = do
	a_parsed_frame <- ZMQHS.Frame.parse test_data
	Prelude.putStrLn "hi"
	