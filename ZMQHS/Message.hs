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
data Message = Message [FrameData]
    deriving (Show)

getMessage :: AP.Parser Message
getMessage = Message <$> parseFrames <?> "getMessage"

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

buildIdentityMessage :: Identity -> BSBuilder.Builder
buildIdentityMessage Anonymous        = buildFrame $ FinalFrame  (BS.pack [])
buildIdentityMessage (Named identity) = buildFrame $ FinalFrame   identity

buildMessage :: Message -> BSBuilder.Builder
buildMessage (Message chunks) =  do
  buildAllFrames chunks

buildAllFrames :: [FrameData] -> BSBuilder.Builder
buildAllFrames (x:[]) =  buildFrame (FinalFrame x)
buildAllFrames (x:xs) = (buildFrame (MoreFrame x)) <> (buildAllFrames xs)