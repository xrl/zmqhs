#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

import Network.URI
import System.Console.CmdArgs

import ZMQHS.Connection as ZC
import ZMQHS.Message

import Control.Monad.Trans
import Control.Monad.Maybe

import qualified Network.Socket as S

data State = Pub | Sub deriving (Typeable, Data, Show)
data Op = Op {state      :: State,
              target     :: String,
              target_uri :: Maybe URI}
            deriving (Typeable, Data, Show)

operation = Op {state      = enum [Pub &= help "Pub",
                                     Sub &= help "Sub"],
                  target     = "tcp://localhost:7890/" &= typ "URI" &= help "Target URI",
                  target_uri = Nothing &= ignore}
              &= program "blaster"
              &= summary "One stop shop for using the ZMQHS library to poke around your ZMQ network"
              -- &= verbosity

main :: IO ()
main = do
  raw_args <- cmdArgs operation
  let app = go_with raw_args {target_uri = parseURI $ target raw_args}
  res <- runMaybeT app
  return ()

--go_with args = do
--  putStrLn (show args)

type MaybeIO = MaybeT IO

go_with :: Op -> MaybeIO ()
go_with args@(Op {state = Pub, target_uri = Just uri}) = do
  lift $ putStrLn "hi"
  (host,part,socktype) <- liftIO $ ZC.uri_parts uri
  --liftIO $ putStrLn (show $ ZC.uri_parts uri)
  --liftIO $ putStrLn (show (host,part,socktype))
  return ()