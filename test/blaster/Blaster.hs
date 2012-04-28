#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

import Network.URI
import System.Console.CmdArgs

import ZMQHS.Connection as ZC
import ZMQHS.Message

import Control.Monad.Trans
import Control.Monad.Maybe

import Data.ByteString.Char8 (pack)

import qualified Network.Socket as S

data State = Pub | Sub deriving (Typeable, Data, Show)
data Op = Op {state      :: State,
              target     :: String,
              target_uri :: Maybe URI,
              identity   :: String,
              payload    :: String}
            deriving (Typeable, Data, Show)

type MaybeIO = MaybeT IO

liftMaybeT :: Monad m => Maybe a -> MaybeT m a
liftMaybeT  = MaybeT . return

operation = Op {state      = enum [Pub &= help "Pub",
                                   Sub &= help "Sub"],
                target     = "tcp://localhost:7890/" &= typ "URI" &= help "Target URI",
                target_uri = Nothing &= ignore,
                identity   = "anonymous" &= typ "ID" &= help "Use name on all messages",
                payload    = "TESTPAYLOAD" &= typ "BYTES" &= help "Message payload"}
             &= program "blaster"
             &= summary "One stop shop for using the ZMQHS library to poke around your ZMQ network"
             -- &= verbosity

main :: IO ()
main = do
  raw_args <- cmdArgs operation
  let app = exec raw_args {target_uri = parseURI $ target raw_args}
  res <- runMaybeT app
  return ()

to_ident :: String -> Identity
to_ident  "anonymous" = Anonymous
to_ident  name        = Named (pack name)

exec :: Op -> MaybeIO ()
exec (Op {state=Pub, target_uri=Just uri, payload=payload, identity=identity}) = do
  ext_uri@(host,part,socktype) <- liftMaybeT $ ZC.uri_parts uri
  sock <- liftIO $ ZC.connect_uri ext_uri typed_id
  sent <- liftIO $ ZC.send sock (Message typed_id [pack payload])
  lift $ putStrLn $ "Sent " ++ (show sent) ++ " bytes"
  return ()
  where typed_id = to_ident identity
exec (Op {state=Sub, target_uri=Just uri, payload=payload, identity=identity}) = do
  ext_uri@(host,part,socktype) <- liftMaybeT $ ZC.uri_parts uri
  sock <- liftIO $ ZC.listen_uri ext_uri typed_id
  liftIO $ ZC.close sock
  return ()
  where typed_id = to_ident identity
exec Op {state = st} = do
  lift $ putStrLn $ "Sorry " ++ (show st) ++ " is not supported yet"
  return ()