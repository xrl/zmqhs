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
(
  frameParser,
  frameData,
  buildFrame,
  debugIt,
  (<>),
  Frame (..),
  FrameData
)
where

import Prelude  hiding (length)

import Control.Applicative hiding (empty)

import Data.Monoid (Monoid, mappend)

import           Data.Word (Word8)
import           Data.Bits
import qualified Data.Attoparsec as AP
import qualified Data.Attoparsec.Binary as APB

import qualified Blaze.ByteString.Builder as BSBuilder
import qualified Blaze.ByteString.Builder.Int as IntBuilder

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

frameLength :: Num a => Frame -> a
frameLength frame = (fromIntegral . BS.length) (frameData frame)

frameParser :: AP.Parser Frame
frameParser = do
  numbytes    <- parseLength
  constructor <- frameConstructor <$> AP.anyWord8
  let payload_size = numbytes - 1
  payload <- AP.take payload_size
  return $ constructor payload

parseLength :: AP.Parser Int
parseLength = do
  first_byte <- fromIntegral <$> AP.anyWord8
  if first_byte == 0xFF
    then fromIntegral <$> APB.anyWord64be
    else return first_byte

frameConstructor :: Word8 -> (FrameData -> Frame)
frameConstructor word
  | word .&. 0x01 == 0x01   = MoreFrame
  | otherwise               = FinalFrame

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

buildFrame :: Frame -> BSBuilder.Builder
buildFrame frame =
  buildLength frame
  <> buildFrameType frame
  <> buildFrameData frame

buildLength :: Frame -> BSBuilder.Builder
buildLength frame
 | (frameLength frame) < 256 = IntBuilder.fromInt8    (1+(frameLength frame))
 | otherwise                 = IntBuilder.fromInt8 0xFF
                            <> IntBuilder.fromInt64be (1+(frameLength frame))

buildFrameType :: Frame -> BSBuilder.Builder
buildFrameType (MoreFrame  _) = IntBuilder.fromInt8 0x01
buildFrameType (FinalFrame _) = IntBuilder.fromInt8 0x00

buildFrameData :: Frame -> BSBuilder.Builder
buildFrameData frame = BSBuilder.fromByteString (frameData frame)

debugIt :: FrameData -> IO ()
debugIt = putStrLn . concatMap (`N.showHex` " ") . BS.unpack
