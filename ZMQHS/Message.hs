-- Copyright:  Xavier Lange, 2011
-- License:    BSD

module ZMQHS.Message
where

import ZMQHS.Frame

import qualified Data.ByteString      as BS
import qualified Data.Binary.Put as P

import Control.Monad
import Control.Applicative hiding (empty)
import Data.Attoparsec((<?>))

data Identity = Anonymous
              | Named BS.ByteString -- can't be empty
    deriving(Show)
data Message = Message Identity [BS.ByteString]
    deriving (Show)

--getMessage :: Message
getMessage = do Message <$> parseIdentity  <*> parseFrames <?> "multipart"
  where
    parseFrames = do
      case frameParser of
        -- The problem is parse frames will return (Frame payload) and
        -- I should only cons the payload part.
        (MoreFrame  payload) -> (payload:) <$> parseFrames
        (FinalFrame payload) -> return [payload]

--getIdentity :: 
parseIdentity = do
  frame <- frameParser
  payload <- case frame of
    MoreFrame payload  -> payload
    FinalFrame payload -> payload
  identify payload

identify :: BS.ByteString -> Identity
identify bs = do
  case BS.length bs of
    0         -> Anonymous
    otherwise -> Named bs

putIdentity :: Identity -> P.PutM ()
putIdentity Anonymous   = putFrame (FinalFrame (BS.pack []))
putIdentity (Named str) = putFrame (FinalFrame str)

putMessage :: Message -> P.Put
putMessage (Message identity chunks) =  do
  putIdentity identity 
  let len = length chunks
  forM_ (take (len-1) chunks) $ \chunk -> do
    putLength (BS.length chunk + 1)
    putFrame (MoreFrame chunk)
  putFrame (FinalFrame $ last chunks)