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
import           Data.Word (Word8, Word64)
import qualified Data.Attoparsec as AP
import qualified Data.Attoparsec.Binary as APB
import Debug.Trace

data FrameCont = FINAL | MORE | BADCONT
    deriving (Show, Eq)
frame_cont 0x00      = FINAL
frame_cont 0x01      = MORE
frame_cont otherwise = BADCONT

data FrameSize = Small Word8 | Jumbo Word64
    deriving (Show)

get_fc = do
    raw_cont <- AP.anyWord8
    guard((frame_cont raw_cont) /= BADCONT) AP.<?> "State must be either MORE or FINAL"
    return raw_cont

parser = do
    frame_length <- AP.anyWord8
    fc <- get_fc
    frame_size <- case frame_length of
        0xFF       ->  Jumbo <$> APB.anyWord64le
        otherwise  ->  return (Small otherwise)
    bs <- case frame_size of
        Jumbo len  -> trace ("Jumbo len: " ++ (show $ len)) (AP.take $ fromIntegral len)
        Small len  -> AP.take (fromIntegral len)
    return (frame_size, fc, bs)