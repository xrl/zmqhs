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
import qualified Data.ByteString as B
import qualified Data.Attoparsec as AP

data FrameCont = FINAL | MORE | BADCONT deriving Eq
frame_cont 0x00      = FINAL
frame_cont 0x01      = MORE
frame_cont otherwise = BADCONT
instance Show FrameCont where
    show FINAL   = "FINAL"
    show MORE    = "MORE"
    show BADCONT = "BADCONT"

parser = do
    frame_length <- AP.anyWord8
    raw_cont     <- AP.anyWord8
    guard((frame_cont raw_cont) /= BADCONT) AP.<?> "State must be either MORE or FINAL"
    body         <- AP.take (fromIntegral frame_length)
    return (frame_length, frame_cont raw_cont, body)