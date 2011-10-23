-- Copyright:  Xavier Lange, 2011
-- License:    BSD

-- From the RFC:
--more-frame  = length more body
--final-frame = length final body
--length      = OCTET / (%xFF 8OCTET)
--more        = %x01
--final       = %x00
--body        = *OCTET

module ZMQHS.Frame
where
import qualified Data.ByteString as B
--import qualified Data.Binary.Strict.Get as G
--import qualified Data.Binary.Get as G
--import qualified Data.Binary.Strict.IncrementalGet as G
import qualified Data.Attoparsec as AP
--import qualified Data.Binary.Put as P
--import           Data.Bits
--import qualified Data.Binary.Strict.BitGet as BG

data FrameState = FINAL | MORE | ERROR

parser = do
	frame_length <- AP.anyWord8
	state_val    <- AP.anyWord8
	body         <- AP.take (fromIntegral frame_length)
	AP.atEnd
	return (frame_length, frame_state state_val, body)
    --frame_length <- G.getWord8
    --state_val    <- G.getWord8
    --body         <- G.getByteString (fromIntegral frame_length)
    --return (frame_length, frame_state state_val, body)

parse = do
    AP.parseTest parser

frame_state 0x00      = FINAL
frame_state 0x01      = MORE
frame_state otherwise = ERROR

instance Show FrameState where
	show FINAL = "FINAL"
	show MORE  = "MORE"
	show ERROR = "ERROR"
