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

data FrameCont = FINAL | MORE | MYSTERIOUS | BADCONT
    deriving (Show, Eq)
frame_cont 0x00      = FINAL
frame_cont 0x01      = MORE
frame_cont 0x7E      = MYSTERIOUS
frame_cont otherwise = BADCONT

get_fc = do
    raw_cont <- frame_cont <$> AP.anyWord8
    guard((raw_cont) /= BADCONT) AP.<?> "State must be either MORE or FINAL"
    return raw_cont

parser = do
    first_byte    <- fromIntegral <$> AP.anyWord8
    rest_of_frame <- case first_byte of
        0xFF ->  fromIntegral <$> APB.anyWord64be
        _    ->  return first_byte
    fc <- get_fc
    let payload_size = rest_of_frame - 1
    payload <- AP.take payload_size
    return (payload_size, fc, payload)

handshake_response_generator = do
    P.putWord8 0x01
    P.putWord8 0x7E
handshake_response = do
    P.runPut handshake_response_generator

--generator (byte:[]) = do
--    P.putWord8 byte
--generator bytes =
--[(P.putWord8)] `ap` bytes
--liftM (P.putWord8) bytes
--mapM (P.putWord8) bytes
--sequence_ (P.putWord8) bytes
--P.putWord8 =<< bytes
--P.putWord8 byte0 >>
--P.putWord8 byte1