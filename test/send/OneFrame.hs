#!/usr/bin/env runhaskell

{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified ZMQHS.Frame     as ZF

import qualified Data.ByteString.Lazy as B
import qualified Data.Attoparsec      as AP
import qualified Data.Binary.Get      as G
import qualified Data.Binary.Put      as P
import Data.Int

import qualified Control.Monad as CM
import Control.Applicative hiding (empty)
import qualified Control.Exception.Base as E

import qualified Network.Socket  as S
import qualified Network.Socket.ByteString as SB
import qualified Network.Socket.ByteString.Lazy as LSB
import qualified Network.BSD     as BSD

import qualified Data.Hex        as DH
import qualified Data.Char       as DC
import qualified Numeric         as N

import qualified System.Exit     as SE


-- http://book.realworldhaskell.org/read/sockets-and-syslog.html
main :: IO ()
main = do
  E.bracket (open_connection "0.0.0.0" "7890")
          S.sClose
          send_and_read
  return ()
  
open_connection :: S.HostName -> S.ServiceName -> IO S.Socket
open_connection servaddr servport = do
  addrinfos <- S.getAddrInfo (Just S.defaultHints) (Just servaddr) (Just servport)
  let servinfo = head addrinfos
  sock <- S.socket (S.addrFamily servinfo) S.Stream S.defaultProtocol
  S.connect sock (S.addrAddress servinfo)
  connected <- S.sIsConnected sock
  case connected of
    True  -> putStrLn "connected!"
    False -> putStrLn "not connected!"
  return sock

send_and_read :: S.Socket -> IO Int64
send_and_read sock = do
  let opening_salvo = B.pack [0x01, 0x7E]
  LSB.send sock opening_salvo
  --stuff <- LSB.recv sock 1024
  --ZF.debug_it stuff
  let outgoing_data = ZF.payload_response (B.pack [65,66,67,68,69])
  LSB.send sock outgoing_data