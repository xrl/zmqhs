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
import Control.Monad
import Control.Applicative hiding (empty)
import           Data.Word (Word8, Word64)
import qualified Data.Attoparsec as AP
import qualified Data.Attoparsec.Binary as APB
import qualified Data.Binary.Put as P
import Debug.Trace

import qualified Data.Hex             as DH
import qualified Data.ByteString.Lazy as B
import qualified Numeric              as N

-- include/zmq.h #define, merge all flags
continuation_flag = 0x01


data FrameCont = FINAL | MORE | MYSTERIOUS | BADCONT
    deriving (Show, Eq)
frame_cont 0x00      = FINAL
frame_cont 0x01      = MORE
frame_cont 0x7E      = MYSTERIOUS
frame_cont otherwise = BADCONT

get_fc = do
    raw_cont <- frame_cont <$> AP.anyWord8
    guard ((raw_cont) /= BADCONT)
        AP.<?> "State must be either MORE or FINAL"
    return raw_cont

parser = do
    first_byte <- fromIntegral <$> AP.anyWord8
    frame_size <- case first_byte of
        0xFF ->  fromIntegral <$> APB.anyWord64be
        _    ->  return first_byte
    fc <- get_fc
    let payload_size = frame_size - 1
    payload <- AP.take payload_size
    return (payload_size, fc, payload)

payload_response a_byte = do
    P.runPut (generator 0x7E a_byte)
generator header body = do
    let header_len = 1
    let body_len   = B.length body
    let len = header_len + body_len
    case len of
        x | x < 255 -> P.putWord8    $ fromIntegral len
        otherwise   -> P.putWord8 0xFF >>
                      (P.putWord64be $ fromIntegral len)
    P.putWord8 header
    P.putLazyByteString body

debug_it = putStrLn . concat . map (flip N.showHex " ") . B.unpack