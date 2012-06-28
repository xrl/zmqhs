module ZMQHS.Async
(
  async,
  wait,
  cancel,
  Async
)
where

import Control.Concurrent
import Control.Exception

data Async = Async ThreadId (MVar ())

async :: IO () -> IO Async
async action = do
  var <- newEmptyMVar
  t <- forkFinally action (putMVar var ())
  return (Async t var)

forkFinally :: IO ()
            -> IO ()
            -> IO ThreadId
forkFinally action and_then = mask $ \restore ->
    forkIO $ finally (restore action) and_then

wait :: Async -> IO ()
wait (Async _ var) = do
  r <- readMVar var
  return ()

--waitSTMThrow :: Async a -> STM a
--waitSTMThrow (Async _ var) = do
--     r <- readTMVar var
--     case r of
--       Left  a -> return a
--       Right e -> throwSTM e

cancel :: Async -> IO ()
cancel (Async t _) = throwTo t ThreadKilled

--withAsync :: IO a -> (Async a -> IO b) -> IO b
--withAsync action inner = bracket (async action) cancel inner