-- Copyright:  Xavier Lange, 2011
-- License:    BSD

-- From the RFC:
-- more-frame  = length more body
-- final-frame = length final body
-- length      = OCTET / (%xFF 8OCTET)
-- more        = %x01
-- final       = %x00
-- body        = *OCTET

module ZMQHS.Frame
where
import Control.Monad (guard)
import Control.Applicative hiding (empty)
import qualified Data.ByteString as B (ByteString)
import Data.Word (Word8, Word64)
import qualified Data.Attoparsec as AP
import qualified Data.Binary.Get as G

data FrameCont = FINAL | MORE | BADCONT
    deriving (Show, Eq)
frame_cont 0x00      = FINAL
frame_cont 0x01      = MORE
frame_cont otherwise = BADCONT

data FrameSize = Small Word8 | Jumbo B.ByteString
    deriving (Show)

--get_flen frame_diff = do
--                if frame_diff == 0x255
--                    then Small frame_diff
--                    else Jumbo (AP.take 8) >>= (G.runGet (G.getWord64be))

get_fc = do
    raw_cont <- AP.anyWord8
    guard((frame_cont raw_cont) /= BADCONT) AP.<?> "State must be either MORE or FINAL"
    return raw_cont

parser = do
    frame_length <- AP.anyWord8
    frame_size <- case frame_length of
        0xFF       ->  Jumbo <$> AP.take 8
        otherwise  ->  return (Small otherwise)
    fc <- get_fc
    bs <- AP.take 8
    return (frame_size, fc, bs)


