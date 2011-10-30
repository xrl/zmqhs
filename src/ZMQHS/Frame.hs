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
import qualified Data.ByteString as B (ByteString, pack, unpack)
import           Data.Word (Word8, Word64)
import qualified Data.Attoparsec as AP
import qualified Data.Attoparsec.Binary as APB
import qualified Data.Binary.Get as G
import           Data.Bits ((.|.), shiftL)
import qualified Data.List as L

data FrameCont = FINAL | MORE | BADCONT
    deriving (Show, Eq)
frame_cont 0x00      = FINAL
frame_cont 0x01      = MORE
frame_cont otherwise = BADCONT

data FrameSize = Small Word8 | Jumbo Word64
    --deriving (Show)

get_fc = do
    raw_cont <- AP.anyWord8
    guard((frame_cont raw_cont) /= BADCONT) AP.<?> "State must be either MORE or FINAL"
    return raw_cont

--to_word64 = L.foldl' (\w b -> (shiftL w 8) .|. fromIntegral b) 0 . B.unpack

--get_word64 = do
--    bs <- AP.take 8
--    return (G.runGet G.getWord64be bs)
--case bs of
--    a -> G.runGet G.getWord64be bs
--    AP.Partial _ -> error "what"
--    AP.Fail _ _ _ -> error "what"

parser = do
    frame_length <- AP.anyWord8
    frame_size <- case frame_length of
        --0xFF       ->  Jumbo <$> (AP.take 8)
        0xFF       ->  Jumbo <$> APB.anyWord64be
        otherwise  ->  return (Small otherwise)
    fc <- get_fc
    bs <- AP.take 8
    --case frame_size of
    --    Small len -> return AP.take $ fromIntegral len
    --    Jumbo len -> return AP.take $ fromIntegral len
    return (frame_size, fc, bs)