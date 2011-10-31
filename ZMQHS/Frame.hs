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

get_fc = do
    raw_cont <- frame_cont <$> AP.anyWord8
    guard((raw_cont) /= BADCONT) AP.<?> "State must be either MORE or FINAL"
    return raw_cont

parser = do
    first_byte <- fromIntegral <$> AP.anyWord8
    frame_size <- case first_byte of
        0xFF ->  fromIntegral <$> APB.anyWord64be
        _    ->  return first_byte
    fc <- get_fc
    body <- AP.take frame_size
    return (frame_size, fc, body)
