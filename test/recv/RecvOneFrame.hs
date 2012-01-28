#!/usr/bin/env runhaskell

{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified ZMQHS.Frame     as ZF

import qualified Data.ByteString as B
import qualified Data.Attoparsec as AP
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P

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

servport = "7890"

-- http://book.realworldhaskell.org/read/sockets-and-syslog.html
main = do
  addrinfos <- S.getAddrInfo
                 (Just (S.defaultHints {S.addrFlags = [S.AI_PASSIVE]}))
                  Nothing
                 (Just servport)
  let servaddr = head addrinfos
  putStrLn (show servaddr)

  sock <- S.socket (S.addrFamily servaddr) S.Stream S.defaultProtocol
  putStrLn (show sock)

  S.bindSocket sock (S.addrAddress servaddr)
  S.listen sock 1

  putStrLn ("Listening on port: " ++ servport)
  E.bracket (S.accept sock)
            (\(reqsock,_) -> S.sClose reqsock)
            (readAllDataNew do_something)

readAllDataNew callback (reqsock,reqaddr) = do
  a_bytestring <- SB.recv reqsock 2048
  CM.unless (B.null a_bytestring)
            -- Do the callback with what we got, then keep going
            (callback a_bytestring reqsock >> (readAllDataNew callback (reqsock,reqaddr)))

--do_something = putStrLn . show . B.unpack
--do_something = putStrLn . show . map (\x -> N.showIntAtBase 16 (DC.intToDigit) x "") . B.unpack
--do_something = putStrLn . concat . map (flip N.showHex "") . B.unpack
do_something str sock = (debug_it str) >> (fake_handle sock)
debug_it              = putStrLn . concat . map (flip N.showHex " ") . B.unpack
fake_handle reqsock   = LSB.send reqsock (ZF.handshake_response)