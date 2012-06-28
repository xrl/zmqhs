module Blaster.Application
where

import ZMQHS
import Control.Monad (forever)
import Data.ByteString.Char8 (pack)

type SReader = IO ()
type SWriter = IO ()
data TheSpammer  = TheSpammer SReader SWriter
data RunningSpammer = RunningSpammer Async Async

--instance RunningApp RunningSpammer where
--  stop (RunningSpammer r w) = do
--    cancel r
--    cancel w

instance App TheSpammer where
  run (TheSpammer reader writer) = do
    r <- reader
    return $ RunningSpammer undefined undefined

-- run $ spammerApp conn 1 "asdfasd"
-- spammerApp :: Connection -> Int -> String -> TheSpammer
-- spammerApp conn rep outgoing = TheSpammer (readAllMessages conn) (writeMessages conn rep outgoing) 

spawnWriter :: Connection -> Int -> String -> IO Async
spawnWriter conn rep outgoing = async $ do
    writeMessages conn rep outgoing

spawnReader :: Connection -> IO Async
spawnReader conn = async (return ())

readAllMessages :: Connection -> IO ()
readAllMessages conn = do
  forever $ do
    msg <- recvMessage conn
    case msg of
      Just _  -> putChar '.'
      Nothing -> error "got nothing?"

writeMessages :: Connection -> Int -> String -> IO ()
writeMessages _    0   _        = return ()
writeMessages conn rep msg = do
  sendMessage conn (Message [pack msg])
  writeMessages conn (rep-1) msg