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
              | Named FrameData
    deriving (Show)
data Message = Message Identity [FrameData]
    deriving (Show)

getMessage :: AP.Parser Message
getMessage = Message <$> parseIdentity  <*> parseFrames <?> "getMessage"

parseFrames :: AP.Parser [FrameData]
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

putIdentityWorker :: (FrameData -> Frame) -> Identity -> P.PutM ()
putIdentityWorker frametype Anonymous   = putFrame (frametype (BS.pack []))
putIdentityWorker frametype (Named str) = putFrame (frametype str)

putInitialIdentity :: Identity -> P.PutM ()
putInitialIdentity = putIdentityWorker (FinalFrame)

putIdentity :: Identity -> P.PutM ()
putIdentity = putIdentityWorker (MoreFrame)

{-
  connecting for the first time
  putIdentity (MoreFrame) (Named "xavier's zmq")
-}

putMessage :: Message -> P.Put
putMessage (Message identity chunks) =  do
  putIdentity identity 
  let len = length chunks
  forM_ (take (len-1) chunks) $ \chunk -> do
    -- putLength (BS.length chunk + 1)
    putFrame (MoreFrame chunk)
  putFrame (FinalFrame $ last chunks)