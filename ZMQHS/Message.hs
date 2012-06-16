-- Copyright:  Xavier Lange, 2011
-- License:    BSD

module ZMQHS.Message
where

import ZMQHS.Frame

import qualified Data.Attoparsec as AP

import qualified Data.ByteString      as BS
-- Put is marked for removal
import qualified Data.Binary.Put as P

import qualified Blaze.ByteString.Builder as BSBuilder
import qualified Blaze.ByteString.Builder.Int as IntBuilder

import Control.Monad
import Data.Monoid (Monoid, mappend)
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

--putIdentityWorker :: (FrameData -> Frame) -> Identity -> P.PutM ()
--putIdentityWorker frametype Anonymous   = putFrame (frametype (BS.pack []))
--putIdentityWorker frametype (Named str) = putFrame (frametype str)

--putInitialIdentity :: Identity -> P.PutM ()
--putInitialIdentity = putIdentityWorker (FinalFrame)

buildIdentityMessage :: Identity -> BSBuilder.Builder
buildIdentityMessage identity = buildMessage (Message identity [])

buildMessage :: Message -> BSBuilder.Builder
buildMessage (Message Anonymous chunks) =  do
  buildAllFrames ((BS.pack []):chunks)
buildMessage (Message (Named name) chunks) =  do
  buildAllFrames (name:chunks)

buildAllFrames :: [FrameData] -> BSBuilder.Builder
buildAllFrames (x:[]) =  buildFrame (FinalFrame x)
buildAllFrames (x:xs) = (buildFrame (MoreFrame x)) <> (buildAllFrames xs)


--let len = length chunks
--forM_ (take (len-1) chunks) $ \chunk -> do
--  -- putLength (BS.length chunk + 1)
--  (putFrame (MoreFrame chunk)) <>
--putFrame (FinalFrame $ last chunks)