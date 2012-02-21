-- Copyright:  Xavier Lange, 2011
-- License:    BSD

-- From the RFC:
-- more-frame  = length more body
-- final-frame = length final body
-- length      = OCTET / (%xFF 8OCTET)
-- more        = %x01
-- final       = %x00
-- body        = *OCTET
{-# LANGUAGE NoMonomorphismRestriction #-}
module ZMQHS.Frame
  (payloadResponse,
   toBS)
where

import Control.Applicative hiding (empty)

import ZMQHS.Message

import           Data.Word (Word8)
import           Data.Bits
import qualified Data.Attoparsec as AP
import Data.Attoparsec((<?>))
import qualified Data.Attoparsec.Binary as APB
import qualified Data.Binary.Put as P
import Control.Monad

import Debug.Trace

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString      as BS
import qualified Numeric              as N

-- include/zmq.h #define, merge all flags

data Frame = MoreFrame  BS.ByteString
           | FinalFrame BS.ByteString
  deriving (Show)

parseFC :: AP.Parser (BS.ByteString -> Frame)
parseFC = do frameCont <$> AP.anyWord8 <?> "frame continuation"
  where frameCont x
         | (.&.) x 0x01 == 0x01 = MoreFrame
         | otherwise            = FinalFrame

{-                 
length      = OCTET / (%xFF 8OCTET)
-}
parseLength :: AP.Parser Int
parseLength = do
  first_byte <- fromIntegral <$> AP.anyWord8
  if first_byte == 0xFF
    then fromIntegral <$> APB.anyWord64be
    else return first_byte

{-
more        = %x01
final       = %x00
body        = *OCTET
-}
frameParser :: AP.Parser Frame
frameParser = do
  frame_size  <- parseLength
  constructor <- parseFC
  let payload_size = frame_size - 1
  payload <- AP.take payload_size
  return (constructor payload)

payloadResponse :: B.ByteString -> B.ByteString
payloadResponse = P.runPut . generator 0x7E

toBS :: Message -> B.ByteString
toBS =  P.runPut . putMessage

putMessage :: Message -> P.Put
putMessage (Message identity chunks) =  do
  putIdentity identity 
  let len = length chunks
  forM_ (take (len-1) chunks) $ \chunk -> do
    putLength (BS.length chunk + 1)
    putMore
    P.putByteString chunk
  -- expanded for clarity
  let lastchunk = last chunks
  putLength   (BS.length lastchunk + 1)
  putFinal
  P.putByteString lastchunk

putIdentity :: Identity -> P.PutM ()
putIdentity Anonymous = P.putWord8 0x01 >> P.putWord8 0x00
putIdentity (Named str) = do
  putLength (BS.length str + 1)
  putFinal
  P.putByteString str
  
putMore :: P.Put
putMore = P.putWord8 0x1

putFinal :: P.Put
putFinal = P.putWord8 0x0  

putLength :: Integral a => a -> P.Put
putLength len
 | len < 255 = P.putWord8    $ fromIntegral len
 | otherwise = P.putWord8 0xFF >> (P.putWord64be $ fromIntegral len)

generator :: Word8 -> B.ByteString -> P.Put
generator header body = do
    let header_len = 1
    let body_len   = B.length body
    let len = header_len + body_len
    putLength len    
    P.putWord8 header
    P.putLazyByteString body

debug_it :: B.ByteString -> IO ()
debug_it = putStrLn . concat . map (flip N.showHex " ") . B.unpack