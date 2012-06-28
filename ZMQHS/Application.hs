module ZMQHS.Application
where

import ZMQHS.Async

class App a where
  run :: (App a, RunningApp b) => a -> IO b

class RunningApp b where
  stop :: (RunningApp b) => b -> IO ()