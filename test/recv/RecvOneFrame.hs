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

--import qualified Data.Hex        as DH
import Numeric as N

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
  procRequests sock prettyPrintZMTP

procRequests sock callback = do
  putStrLn "Waiting for new connection"
  (reqsock, reqaddr) <- S.accept sock
  putStrLn "Handling new connection"
  readAllData reqsock callback

prettyPrintZMTP "" = return ()
prettyPrintZMTP s  = putStrLn s

readAllData reqsock callback = do
  putStrLn "reading some data"
  a_bytestring       <- SB.recv reqsock 2048
  callback $ show a_bytestring
  readAllData reqsock callback