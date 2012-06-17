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

{-# LANGUAGE OverloadedStrings #-}

module ZMQHS.Connection
(
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
type MessageSink   = Sink Message (ResourceT IO) ()
data Connection    = Connection MessageSource MessageSink S.Socket
data Server        = Server Identity S.Socket

greetedSink :: S.Socket -> Identity -> IO MessageSink
greetedSink sock identity = do
  -- Send our identity
  let ungreeted_sink = builderToByteString =$ sinkSocket sock
  _ <- runResourceT $ (identitySource identity) $$ ungreeted_sink
  let greeted_sink   = messageToBuilderConduit =$ ungreeted_sink
  return greeted_sink

greetedSource :: S.Socket -> IO (MessageSource,Identity)
greetedSource sock = do
  -- Setup the incoming data stream
  let ungreeted_unid_source = sourceSocket sock
    -- This is a strict interpretation of how the other ZMQ respondent will behave. Pattern match failure if they're deviant!
  let get_identity = ungreeted_unid_source $$ (sinkParser getMessage)
  (Message (frame:[])) <- runResourceT $ get_identity
  let greeted_message_source   = ungreeted_unid_source $= messageParserConduit
  return (greeted_message_source,pureIdentity frame)

--  (MonadIO m, MonadUnsafeIO m, MonadThrow m) => 
client :: ConnSpec -> Identity -> IO Connection
client (servaddr,servport,socktype) identity = do
  -- Setup the outgoing socket using basic network commands
  addrinfos <- S.getAddrInfo (Just S.defaultHints) (Just servaddr) (Just servport)
  let servinfo = head addrinfos
  sock <- S.socket (S.addrFamily servinfo) socktype S.defaultProtocol
  S.connect sock (S.addrAddress servinfo)

  -- Setup the outgoing data stream
  let ungreeted_sink = builderToByteString =$ sinkSocket sock
  _ <- runResourceT $ (identitySource identity) $$ ungreeted_sink
  let greeted_sink   = messageToBuilderConduit =$ ungreeted_sink

  -- Setup the incoming data stream
  let ungreeted_unid_source = sourceSocket sock
    -- This is a strict interpretation of how the other ZMQ respondent will behave. Pattern match failure if they're deviant!
  let get_identity = ungreeted_unid_source $$ (sinkParser getMessage)
  (Message (their_ident:[])) <- runResourceT $ get_identity
  putStrLn $ "The server's identity: " ++ show their_ident
  let ungreeted_source = ungreeted_unid_source

  let greeted_message_source   = ungreeted_source $= messageParserConduit

  return $ Connection greeted_message_source greeted_sink sock

connspec :: ConnSpec
connspec =
  case spec "tcp://0.0.0.0:7890" of
    Just specy -> specy
    Nothing -> error "never gonna happen!"

server :: ConnSpec -> Identity -> IO Server
server blah@(hostname,servname,_) identity = do
  socket <- bindPort (read servname) (Host hostname)
  putStrLn $ show socket
  return $ Server identity socket

accept :: Server -> IO Connection
accept (Server identity listenersock) = do
  (sock, _) <- S.accept listenersock
  greeted_sink <- greetedSink sock identity
  (greeted_source,_) <- greetedSource sock
  return $ Connection greeted_source greeted_sink sock

messageParserConduit :: Pipe ByteString Message (ResourceT IO) ()
messageParserConduit  = sequence $ sinkParser getMessage

identitySource :: Identity -> Source (ResourceT IO) Builder
identitySource identity = yield $ buildIdentityMessage identity

messageToBuilderConduit :: Monad m => Conduit Message m Builder
messageToBuilderConduit = CL.map buildMessage

recvMessage :: Connection -> IO (Maybe Message)
recvMessage     (Connection src _ _)      = runResourceT $ src $$ await

sendMessage :: Connection -> Message -> IO ()
sendMessage     (Connection _ snk _)  msg = runResourceT $ yield msg $$ snk

closeConnection :: Connection -> IO ()
closeConnection (Connection _ _ sock)     = S.sClose sock
