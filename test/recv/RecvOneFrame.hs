#!/usr/bin/env runhaskell

{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Data.ByteString as B (pack)
import qualified Data.Attoparsec as AP
import qualified ZMQHS.Frame     as ZF
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P

import qualified Network.Socket  as S
import qualified Network.Socket.ByteString as SB
import qualified Network.BSD     as BSD

servport = "8765"
--servaddr = "localhost"

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
  procRequests sock (\x -> putStrLn (show x))
  where
    procRequests sock callback = do
      (reqsock, reqaddr) <- S.accept sock
      a_bytestring       <- SB.recv reqsock 2048
      callback a_bytestring