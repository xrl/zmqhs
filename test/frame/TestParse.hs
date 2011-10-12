import Data.ByteString.Char8 as B
import Data.Binary.Get as G
import Data.Binary.Put as P
import ZMQHS.Frame ()

test_data = do
	P.putWord8 0x01

parse = do
    id <- G.getWord16be
    return id

main = do
	Prelude.putStrLn "hi"
	