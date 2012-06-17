#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

import System.Console.CmdArgs (cmdArgs, Data, Typeable, help, enum, (&=), typ, ignore, program, summary)

import qualified ZMQHS as Z

import Control.Monad.Trans
import Control.Monad.Maybe
import Control.Concurrent

import Data.ByteString.Char8 (pack)

import Data.Conduit
import Data.Conduit.Network (sourceSocket)
import Data.Conduit.Attoparsec

type MaybeIO = MaybeT IO

liftMaybeT :: Monad m => Maybe a -> MaybeT m a
liftMaybeT  = MaybeT . return

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

operation = CmdOp {mode_        = enum [Pub &= help "Pub",
                                        Sub &= help "Sub"],
                   target_      = "" &= typ "URI" &= help "Target URI spec",
                   identity_    = "anonymous" &= typ "ID" &= help "Use name on all messages",
                   payload_     = "TESTPAYLOAD" &= typ "BYTES" &= help "Message payload",
                   repetitions_ = 1 &= typ "REPEAT" &= help "Repititions"}
                &= program "blaster"
                &= summary "One stop shop for using the ZMQHS library to poke around your ZMQ network"
                -- &= verbosity

main :: IO ()
main = do
  raw_args <- cmdArgs operation
  let args = op_from_cmd_op raw_args
  case (target_spec args) of
    Just connspec  -> exec args
    Nothing        -> putStrLn "Target spec must be a valid spec"
  return ()

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
exec Op {mode=Sub, target_spec=Just target_spec, payload=payload, identity=identity} = do
  return ()
exec Op {mode=Pub, target_spec=Just target_spec, payload=payload, identity=identity,repetitions=rep} = do
  conn <- Z.client target_spec (to_ident identity)
  thread <- forkIO $ readAllMessages conn
  foldr (>>) (return ()) (take rep $ repeat (Z.sendMessage conn (Z.Message [pack payload])))
  return ()
exec Op {target_spec=Nothing} = do
  putStrLn "Sorry, please enter a valid target specification (e.g., \"tcp://localhost:7890\")"

readAllMessages :: Z.Connection -> IO ()
readAllMessages conn = do
  foldr (>>) (return ()) $ repeat $ do
    msg <- Z.recvMessage conn
    case msg of
      Just msg -> print "."
      Nothing  -> return ()