#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

import System.Console.CmdArgs (cmdArgs, Data, Typeable, help, enum, (&=), typ, program, summary)

import qualified ZMQHS as Z

import Control.Exception
import Control.Concurrent

import Data.ByteString.Char8 (pack)

data Mode = Pub | Sub deriving (Typeable, Data, Show)
data CmdOp = CmdOp {mode_        :: Mode,
                    target_      :: String,
                    identity_    :: String,
                    payload_     :: String,
                    repetitions_ :: Int}
     deriving (Typeable, Data, Show)

data Op = Op {mode        :: Mode,
              target      :: String,
              target_spec :: Maybe Z.ConnSpec,
              identity    :: String,
              payload     :: String,
              repetitions :: Int}
     deriving (Show)

operation :: CmdOp
operation = CmdOp {mode_        = enum [Pub &= help "Pub",
                                        Sub &= help "Sub"],
                   target_      = "" &= typ "URI" &= help "Target URI spec",
                   identity_    = "anonymous" &= typ "ID" &= help "Use name on all messages",
                   payload_     = "TESTPAYLOAD" &= typ "BYTES" &= help "Message payload",
                   repetitions_ = 1 &= typ "REPEAT" &= help "Repititions"}
                &= program "blaster"
                &= summary "One stop shop for poking around your ZMQ network"
                -- &= verbosity

main :: IO ()
main = do
  raw_args <- cmdArgs operation
  let args = op_from_cmd_op raw_args
  case (target_spec args) of
    Just _  -> exec args
    Nothing -> putStrLn "Target spec must be a valid spec"
  return ()

op_from_cmd_op :: CmdOp -> Op
op_from_cmd_op CmdOp{mode_=m,target_=t,identity_=i,payload_=p,repetitions_=r} =
  Op {mode        = m,
      target      = t,
      target_spec = Z.spec t,
      identity    = i,
      payload     = p,
      repetitions = r}

to_ident :: String -> Z.Identity
to_ident  "anonymous" = Z.Anonymous
to_ident  name        = Z.Named (pack name)

exec :: Op -> IO ()
exec Op {mode=Sub, target_spec=Just targ, payload=outgoing, identity=identstr} = do
  serv <- Z.server targ (to_ident identstr)
  conn <- Z.accept serv
  _ <- writeMessages conn 1 outgoing
  return ()
exec Op {mode=Pub, target_spec=Just spec, payload=outgoing, identity=identstr,repetitions=rep} = do
  conn <- Z.client spec (to_ident identstr)
  _ <- forkIO $ readAllMessages conn
  _ <- writeMessages conn rep outgoing
  return ()
exec Op {target_spec=Nothing} = do
  return ()

readAllMessages :: Z.Connection -> IO ()
readAllMessages conn = do
  foldr (>>) (return ()) $ repeat $ do
    msg <- Z.recvMessage conn
    case msg of
      Just _  -> putChar '.'
      Nothing -> putStrLn "got nothing?"

writeMessages :: Z.Connection -> Int -> String -> IO ()
writeMessages conn rep outgoing = do
  foldr (>>) (return ()) (take rep $ repeat (Z.sendMessage conn (Z.Message [pack outgoing])))

--myForkIO :: IO () -> IO (MVar ())
--myForkIO io = do
--  mvar <- newEmptyMVar
--  _ <- forkIO (io `finally` putMVar mvar ())
--  return mvar