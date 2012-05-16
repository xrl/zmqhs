#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

import System.Console.CmdArgs (cmdArgs, Data, Typeable, help, enum, (&=), typ, ignore, program, summary)

import qualified ZMQHS as Z

import Control.Monad.Trans
import Control.Monad.Maybe

import Data.ByteString.Char8 (pack)

import Data.Conduit
import Data.Conduit.Network (sourceSocket)
import Data.Conduit.Attoparsec

type MaybeIO = MaybeT IO

liftMaybeT :: Monad m => Maybe a -> MaybeT m a
liftMaybeT  = MaybeT . return

data Mode = Pub | Sub deriving (Typeable, Data, Show)
data CmdOp = CmdOp {mode_      :: Mode,
                    target_    :: String,
                    identity_  :: String,
                    payload_   :: String}
     deriving (Typeable, Data, Show)
data Op = Op {mode        :: Mode,
              target      :: String,
              target_spec :: Maybe Z.ConnSpec,
              identity    :: String,
              payload     :: String}
     deriving (Show)

operation = CmdOp {mode_        = enum [Pub &= help "Pub",
                                        Sub &= help "Sub"],
                   target_      = "" &= typ "URI" &= help "Target URI spec",
                   identity_    = "anonymous" &= typ "ID" &= help "Use name on all messages",
                   payload_     = "TESTPAYLOAD" &= typ "BYTES" &= help "Message payload"}
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

op_from_cmd_op CmdOp{mode_=m,target_=t,identity_=i,payload_=p} =
  Op {mode        = m,
      target      = t,
      target_spec = Z.spec t,
      identity    = i,
      payload     = p}

to_ident :: String -> Z.Identity
to_ident  "anonymous" = Z.Anonymous
to_ident  name        = Z.Named (pack name)

--exec :: Op -> MaybeIO ()
--exec Op {mode=Pub, target_spec=Just target_spec, payload=payload, identity=identity} = do
--  sock <- liftIO $ Z.connect target_spec typed_id
--  sent <- liftIO $ Z.send sock (Z.Message typed_id [pack payload])
--  lift $ putStrLn $ "Sent " ++ (show sent) ++ " bytes"
--  return ()
--  where typed_id = to_ident identity

exec :: Op -> IO ()
exec Op {mode=Pub, target_spec=Just target_spec, payload=payload, identity=identity} = do
  Z.Client sock <- Z.connect target_spec typed_id
  putStrLn $ "sock: " ++ (show sock)
  --req  <- runResourceT $ (sourceSocket sock) $$ (sinkParser Z.getMessage)
  --putStrLn "Hello conduit... got " ++ (show req)
  return ()
  where typed_id = to_ident identity

--exec Op {mode=Sub, target_spec=Just target_spec, payload=payload, identity=identity} = do
--  sock <- liftIO $ Z.listen target_spec typed_id
--  liftIO $ Z.close sock
--  return ()
--  where typed_id = to_ident identity

--exec Op {mode = mode} = do
--  lift $ putStrLn $ "Sorry " ++ (show mode) ++ " is not supported yet"
--  return ()