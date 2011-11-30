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
import qualified Network.BSD     as BSD

import qualified Data.Hex        as DH
import qualified Data.Char       as DC
import qualified Numeric         as N

servport = "8765"

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
            (readAllDataNew (putStrLn . show . (map (\x -> x " ")) . (map (N.showIntAtBase 2 DC.intToDigit)) . (map (fromEnum)) . show))

readAllDataNew callback (reqsock,reqaddr) = do
  a_bytestring <- SB.recv reqsock 2048
  CM.unless (B.null a_bytestring)
            (callback a_bytestring >> (readAllDataNew callback (reqsock,reqaddr)))