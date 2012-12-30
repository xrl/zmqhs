-- Copyright:  Xavier Lange, 2011
-- License:    BSD
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module ZMQHS.Connection
(
  startServer,
  accept,
  client,
  Connection(..),
  recvMessage,
  sendMessage,
  closeConn,
  stopServer,
  yieldIdentity
)
where
import ZMQHS.Message   
import ZMQHS.ConnSpec
import Prelude hiding (sequence)
import qualified Network.Socket as S
import           Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Conduit.Attoparsec
import           Data.Conduit.Network
import           Data.Conduit.Blaze

import qualified Blaze.ByteString.Builder as BSBuilder

type MessageSource = Source (ResourceT IO) Message
instance Show MessageSource where
  show _ = "MessageSource"
type MessageSink   = Sink Message (ResourceT IO) ()
instance Show MessageSink where
  show _ = "MessageSink"
data Connection    = Connection MessageSource MessageSink S.Socket
  deriving (Show)
data Server        = Server Identity S.Socket
  deriving (Show)

greetedSink :: S.Socket -> Identity -> IO MessageSink
greetedSink sock identity = do
  let builderSink = builderToByteString =$ sinkSocket sock
  runResourceT $ (yieldIdentity identity) $$ builderSink
  return $ CL.map buildMessage =$ builderSink

--yieldIdentity :: Identity -> Pipe Message Message BSBuilder.Builder () () ()
yieldIdentity identity = yield $ buildIdentityMessage identity

greetedSource :: S.Socket -> IO (MessageSource,Identity)
greetedSource sock = do
  let ungreetedUnidSource = sourceSocket sock
  identity <- runResourceT (ungreetedUnidSource $$ sinkParser parseIdentity)
  let greetedMessageSource   = ungreetedUnidSource $= CL.sequence (sinkParser getMessage)
  return (greetedMessageSource,identity)

client :: ConnSpec -> Identity -> IO Connection
client (servaddr,servport,socktype) identity = do
  addrinfos <- S.getAddrInfo (Just S.defaultHints) (Just servaddr) (Just servport)
  let servinfo = head addrinfos
  sock <- S.socket (S.addrFamily servinfo) socktype S.defaultProtocol
  S.connect sock (S.addrAddress servinfo)

  greeted_sink <- greetedSink sock identity
  (greeted_source,_) <- greetedSource sock

  return $ Connection greeted_source greeted_sink sock

startServer :: ConnSpec -> Identity -> IO Server
startServer (hostname,servname,_) identity = do
  socket <- bindPort (read servname) (Host hostname)
  return $ Server identity socket

accept :: Server -> IO Connection
accept (Server identity listenersock) = do
  (sock, _) <- S.accept listenersock
  greeted_sink <- greetedSink sock identity
  (greeted_source,_) <- greetedSource sock
  return $ Connection greeted_source greeted_sink sock

recvMessage :: Connection -> IO (Maybe Message)
recvMessage (Connection src _ _)     = runResourceT $ src $$ await

sendMessage :: Connection -> Message -> IO ()
sendMessage (Connection _ snk _) msg = runResourceT $ yield msg $$ snk

closeConn :: Connection -> IO ()
closeConn   (Connection _ _ sock)     = S.sClose sock

stopServer :: Server -> IO ()
stopServer  (Server _ sock)           = S.sClose sock