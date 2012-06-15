-- Copyright:  Xavier Lange, 2011
-- License:    BSD

module ZMQHS.Message
where

import ZMQHS.Frame

import qualified Data.Attoparsec as AP

import qualified Data.ByteString      as BS
import qualified Data.Binary.Put as P

import Control.Monad
import Control.Applicative hiding (empty)
import Data.Attoparsec((<?>))

data Identity = Anonymous
              | Named BS.ByteString
    deriving (Show)
data Message = Message Identity [BS.ByteString]
    deriving (Show)

getMessage :: AP.Parser Message
getMessage = Message <$> parseIdentity  <*> parseFrames <?> "getMessage"

parseFrames :: AP.Parser [BS.ByteString]
parseFrames = do
  frame <- frameParser
  case frame of
    (MoreFrame  payload) -> (payload :) <$> (parseFrames)
    (FinalFrame payload) -> return [payload]

parseIdentity :: AP.Parser Identity
parseIdentity = do
  frame <- frameParser
  return $ (identify . frameData) frame
  where
    identify bs = case BS.length bs of
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
    -- putLength (BS.length chunk + 1)
    putFrame (MoreFrame chunk)
  putFrame (FinalFrame $ last chunks)