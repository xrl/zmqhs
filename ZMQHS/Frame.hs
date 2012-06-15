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
(
  frameParser,
  frameData,
  putFrame,
  debugIt,
  Frame (..)
)
where

import Prelude  hiding (length)

import Control.Applicative hiding (empty)

import           Data.Word (Word8)
import           Data.Bits
import qualified Data.Attoparsec as AP
import Data.Attoparsec((<?>))
import qualified Data.Attoparsec.Binary as APB
import qualified Data.Binary.Put as P

import Debug.Trace

import qualified Data.ByteString      as BS
import qualified Numeric              as N

-- include/zmq.h #define, merge all flags

type FrameData = BS.ByteString

data Frame = MoreFrame  FrameData
           | FinalFrame FrameData
  deriving (Show)

frameData :: Frame -> FrameData
frameData (MoreFrame  payload) = payload
frameData (FinalFrame payload) = payload

frameParser :: AP.Parser Frame
frameParser = do
  numbytes    <- length
  constructor <- (<$>) frameConstructor AP.anyWord8
  let payload_size = numbytes - 1
  payload <- AP.take payload_size
  return $ constructor payload

length :: AP.Parser Int
length = do
  first_byte <- fromIntegral <$> AP.anyWord8
  if first_byte == 0xFF
    then fromIntegral <$> APB.anyWord64be
    else return first_byte

frameConstructor :: Word8 -> (FrameData -> Frame)
frameConstructor word
  | (.&.) word 0x01 == 0x01 = MoreFrame
  | otherwise               = FinalFrame

putFrame :: Frame -> P.Put
putFrame (MoreFrame bs) = do
  putLength (1+BS.length bs)
  P.putWord8 0x01
  P.putByteString bs
putFrame (FinalFrame bs) = do
  putLength (1+BS.length bs)
  P.putWord8 0x00
  P.putByteString bs 

putLength :: Integral a => a -> P.Put
putLength len
 | len < 255 = P.putWord8       $ fromIntegral len
 | otherwise = P.putWord8 0xFF >> (P.putWord64be . fromIntegral) len

debugIt :: BS.ByteString -> IO ()
debugIt = putStrLn . concatMap (`N.showHex` " ") . BS.unpack