-- Copyright:  Xavier Lange, 2011
-- License:    BSD

{-
    ZMQ ABNF
    connection  = greeting content
    greeting    = anonymous / identity
    anonymous   = %x01 final
    identity    = length final (%x01-ff) *OCTET
    
    message     = *more-frame final-frame
    more-frame  = length more body
    final-frame = length final body
    length      = OCTET / (%xFF 8OCTET)
    more        = %x01
    final       = %x00
    body        = *OCTET
-}

module ZMQHS.Connection
where
import ZMQHS.Frame      as X
import ZMQHS.Message    as X
import ZMQHS.ConnSpec   as X
-- I want type sigs to be more general. How can I get rid of this?
import GHC.Int
import Control.Applicative hiding (empty)
import Control.Monad.IO.Class
import qualified Network.Socket as S
import qualified Data.Binary.Put as P
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Network
import           Data.ByteString (ByteString)

data Connection m = Connection (IO (m Message)) (IO (Sink ByteString IO ()))

--class ZMQSocket conn where
--    close :: conn -> IO ()
--instance ZMQSocket Connection where
--    close (Connection spec sock) = S.sClose sock
--instance ZMQSocket Listener where
--    close (Listener spec sock)   = S.sClose sock

connection = undefined

--connection :: ConnSpec -> Identity -> Connection
--connection connspec id =
--  return $ Connection (source connspec id) (sink connspec id)

--source = undefined

--source :: MonadIO m => ConnSpec -> Identity -> IO (Source m ByteString)
source :: ConnSpec -> Identity -> IO (Source IO ByteString)
source connspec@(servaddr,servport,socktype) id = do
  addrinfos <- S.getAddrInfo (Just S.defaultHints) (Just servaddr) (Just servport)
  let servinfo = head addrinfos
  sock <- S.socket (S.addrFamily servinfo) socktype S.defaultProtocol
  S.connect sock (S.addrAddress servinfo)
  return $ sourceSocket sock

connspec = case spec "tcp://localhost:4000" of
  Just a -> a
  Nothing -> error "no way that didn't work"

sink :: ConnSpec -> Identity -> IO (Sink ByteString IO ())
sink connspec@(servaddr,servport,socktype) id = do
  addrinfos <- S.getAddrInfo (Just S.defaultHints) (Just servaddr) (Just servport)
  let servaddrinfo = head addrinfos
  sock <- S.socket (S.addrFamily servaddrinfo) socktype S.defaultProtocol
  S.bindSocket sock (S.addrAddress servaddrinfo)
  S.listen sock 1
  return $ sinkSocket sock

{-
    The main way of getting a Connection. If you want more fine-grained control
    over socket settings feel free to roll your own.
-}
--connect :: ConnSpec -> Identity -> IO Connection
--connect connspec@(servaddr,servport,socktype) id = do
--  addrinfos <- S.getAddrInfo (Just S.defaultHints) (Just servaddr) (Just servport)
--  let servinfo = head addrinfos
--  sock <- S.socket (S.addrFamily servinfo) socktype S.defaultProtocol
--  S.connect sock (S.addrAddress servinfo)
--  let clientspec = ClientSpec connspec id
--  return $ Connection clientspec sock

{-
    Send data as you please
-}
--send :: Connection -> Message -> IO GHC.Int.Int64
--send (Connection cspec m) msg = do
--    let outgoing = P.runPut $ putMessage msg
--    LSB.send sock outgoing

--listen :: ConnSpec -> Identity -> IO Listener
--listen connspec@(servaddr,servport,socktype) id = do
--    addrinfos <- S.getAddrInfo (Just S.defaultHints) (Just servaddr) (Just servport)
--    let servaddrinfo = head addrinfos
--    sock <- S.socket (S.addrFamily servaddrinfo) socktype S.defaultProtocol
--    S.bindSocket sock (S.addrAddress servaddrinfo)
--    S.listen sock 1
--    let servspec = ServerSpec connspec id
--    return $ Listener servspec sock

--accept :: Socket -> IO (S.Socket, S.SockAddr)
--accept (Server serversock _) = S.accept serversock