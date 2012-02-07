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

import Control.Applicative hiding (empty)

import           Data.Word (Word8)
import qualified Data.Attoparsec as AP
import Data.Attoparsec((<?>))
import qualified Data.Attoparsec.Binary as APB
import qualified Data.Binary.Put as P
import Control.Monad

import Debug.Trace

-- import qualified Data.Hex             as DH
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString      as BS
import qualified Numeric              as N

-- include/zmq.h #define, merge all flags
continuation_flag :: Integer
continuation_flag = 0x01



data FrameCont = FINAL | MORE  | BADCONT Word8
    deriving (Show, Eq)

-- wow, this is horrible!
frame_cont :: Word8 -> FrameCont 
frame_cont x         
  | x `mod` 2 == 0   = FINAL
  | otherwise =        MORE

data Message = Message Identity [BS.ByteString]
  deriving (Show)
data Identity = Anonymous
              | Named BS.ByteString -- can't be empty
  deriving(Show)
parseFC :: AP.Parser FrameCont
parseFC =  frame_cont <$> AP.anyWord8 <?> "frame continuation"

parseFinal :: AP.Parser ()
parseFinal = do x <- AP.anyWord8 
                guard (x `mod` 2 == 0) 
                return () <?> "final"

parseMore :: AP.Parser Word8
parseMore = AP.word8 0x01  <?> "more"

parseIdentity :: AP.Parser  Identity
parseIdentity = do
  len <- parseLength
  _ <- parseFinal
  (if len == 1
     -- anonymous, let's record that
     then trace "anon" $  return Anonymous 
     else trace "named" $  Named <$> AP.take (len - 1)) <?> "identity"
      
                                
parseMultipart :: AP.Parser Message
parseMultipart = Message <$> parseIdentity  <*> parseFrames <?> "multipart"
  where
    parseFrames = do
      (_, cont, payload) <- frameParser
      case cont of
        FINAL -> return [payload]
        MORE  -> (payload:) <$> parseFrames
        _     -> error "unexpected"
          
{-                 
length      = OCTET / (%xFF 8OCTET)
-}
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

frameParser :: AP.Parser (Int, FrameCont, BS.ByteString)
frameParser = do
  frame_size <- parseLength
  fc <- parseFC
  let payload_size = frame_size - 1
  payload <- AP.take payload_size
  return (payload_size, fc, payload)

payload_response :: B.ByteString -> B.ByteString
payload_response = P.runPut . generator 0x7E

structured_response = P.runPut . msgGen

msgGen :: Message -> P.PutM ()
msgGen (Message id chunks) =  do
  
  undefined

generator :: Word8 -> B.ByteString -> P.PutM ()
generator header body = do
    let header_len = 1
    let body_len   = B.length body
    let len = header_len + body_len
    if len < 255
      then P.putWord8    $ fromIntegral len
      else P.putWord8 0xFF >> (P.putWord64be $ fromIntegral len)
    P.putWord8 header
    P.putLazyByteString body

debug_it :: B.ByteString -> IO ()
debug_it = putStrLn . concat . map (flip N.showHex " ") . B.unpack