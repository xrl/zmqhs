-- Copyright:  Xavier Lange, 2011
-- License:    BSD
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module ZMQHS.Connection
(
  server,
  accept,
  client,
  Connection(..),
  recvMessage,
  sendMessage,
  closeConnection
)
where
import ZMQHS.Frame
import ZMQHS.Message   
import ZMQHS.ConnSpec
import Prelude hiding (sequence)
import qualified Network.Socket as S
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Conduit.Attoparsec
import           Data.Conduit.Network
import           Data.Conduit.Blaze
import           Data.ByteString (ByteString)
import           Blaze.ByteString.Builder

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
  let ungreeted_sink = builderToByteString =$ sinkSocket sock
  runResourceT $ (yield $ buildIdentityMessage identity) $$ ungreeted_sink
  let greeted_message_sink   = CL.map buildMessage =$ ungreeted_sink
  return greeted_message_sink

greetedSource :: S.Socket -> IO (MessageSource,Identity)
greetedSource sock = do
  let ungreeted_unid_source = sourceSocket sock
    -- This is a strict interpretation of how the other ZMQ respondent will behave. Pattern match failure if they're deviant!
  let get_identity = ungreeted_unid_source $$ sinkParser getMessage
  (Message (frame:[])) <- runResourceT get_identity
  let greeted_message_source   = ungreeted_unid_source $= sequence (sinkParser getMessage)
  return (greeted_message_source,pureIdentity frame)

client :: ConnSpec -> Identity -> IO Connection
client (servaddr,servport,socktype) identity = do
  addrinfos <- S.getAddrInfo (Just S.defaultHints) (Just servaddr) (Just servport)
  let servinfo = head addrinfos
  sock <- S.socket (S.addrFamily servinfo) socktype S.defaultProtocol
  S.connect sock (S.addrAddress servinfo)

  greeted_sink <- greetedSink sock identity
  (greeted_source,_) <- greetedSource sock

  return $ Connection greeted_source greeted_sink sock

server :: ConnSpec -> Identity -> IO Server
server (hostname,servname,_) identity = do
  socket <- bindPort (read servname) (Host hostname)
  return $ Server identity socket

accept :: Server -> IO Connection
accept (Server identity listenersock) = do
  (sock, _) <- S.accept listenersock
  greeted_sink <- greetedSink sock identity
  (greeted_source,_) <- greetedSource sock
  return $ Connection greeted_source greeted_sink sock

recvMessage :: Connection -> IO (Maybe Message)
recvMessage     (Connection src _ _)      = runResourceT $ src $$ await

sendMessage :: Connection -> Message -> IO ()
sendMessage     (Connection _ snk _)  msg = runResourceT $ yield msg $$ snk

closeConnection :: Connection -> IO ()
closeConnection (Connection _ _ sock)     = S.sClose sock
