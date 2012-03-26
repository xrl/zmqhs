-- Copyright:  Xavier Lange, 2011
-- License:    BSD

-- From the RFC:
-- more-frame  = length more body
-- final-frame = length final body
-- length      = OCTET / (%xFF 8OCTET)
-- more        = %x01
-- final       = %x00
-- body        = *OCTET

module ZMQHS.Connection
(
    connect,
    send,
    listen,
    S.SocketType(..),
    Connection(..)
)
where
import ZMQHS.Frame
import ZMQHS.Message
-- I want type sigs to be more general. How can I get rid of this?
import GHC.Int
import Control.Monad (guard)
import Control.Applicative hiding (empty)
import           Data.Word (Word8, Word64)
import qualified Data.Attoparsec as AP
import qualified Data.Attoparsec.Binary as APB
import qualified Network.Socket  as S
import qualified Network.Socket.ByteString.Lazy as LSB
import qualified Data.Binary.Put as P

data Connection = Client S.Socket Identity
                | Server S.Socket Identity

getSocket :: Connection -> S.Socket
getSocket (Client sock _) = sock
getSocket (Server sock _) = sock

{-
    Hand-waving to make it easy for you to just open a TCP ZMQ socket
-}
connect_tcp :: S.HostName -> S.ServiceName -> Identity -> IO Connection
connect_tcp servaddr servport id = connect servaddr servport S.Stream id

{-
    The main way of getting a Connection. If you want more fine-grained control
    over socket settings feel free to roll your own.
-}
connect :: S.HostName -> S.ServiceName -> S.SocketType -> Identity -> IO Connection
connect servaddr servport socktype id = do
  addrinfos <- S.getAddrInfo (Just S.defaultHints) (Just servaddr) (Just servport)
  let servinfo = head addrinfos
  sock <- S.socket (S.addrFamily servinfo) socktype S.defaultProtocol
  S.connect sock (S.addrAddress servinfo)
  return $ Client sock id

{-
    Send data as you please
-}
send :: Connection -> Message -> IO GHC.Int.Int64
send (Client sock id) msg = do
    let outgoing = P.runPut $ putMessage msg
    LSB.send sock outgoing
send (Server sock id) msg = do
    let outgoing = P.runPut $ putMessage msg
    LSB.send sock outgoing

list_tcp :: S.HostName -> S.ServiceName -> Identity -> IO Connection
list_tcp servaddr servport id = listen servaddr servport S.Stream id

listen :: S.HostName -> S.ServiceName -> S.SocketType -> Identity -> IO Connection
listen servaddr servport socktype id = do
    addrinfos <- S.getAddrInfo (Just S.defaultHints) (Just servaddr) (Just servport)
    -- TODO servaddrinfo could be []. This will fail at runtime. Perhaps IO (Maybe Connection) would be better?
    let servaddrinfo = head addrinfos
    sock <- S.socket (S.addrFamily servaddrinfo) socktype S.defaultProtocol
    S.bindSocket sock (S.addrAddress servaddrinfo)
    S.listen sock 1
    return $ Server sock id

accept :: Connection -> Connection
accept (Server serversock _) = do 
    clientsock <- S.Accept serversock
    first_frame < - readFrame clientSock 
    return $ Connection clientsock 

readFrame :: S.Socket -> IO (Maybe Frame)
readFrame sock = do
    framepart <- LSB.recv sock 2048
    readFramePart sock $ frameParser framepart
readFramePart sock (Fail unconsumed ctx reason) = do
    return Nothing
readFramePart sock (Partial cont) = do
    moredata <- LSB.recv sock 2048
    readFramePart sock $ cont moredata
readFramePart sock (Done leftover frame) = do
    return frame

--recv :: Connection -> Message
--recv (Server sock id) = do


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