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
-- I want type sigs to be more general. How can I get rid of this?
import GHC.Int
import Prelude hiding (sequence)
import Control.Applicative hiding (empty)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Network.Socket as S
import qualified Data.Binary.Put as P
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Conduit.Attoparsec
import           Data.Conduit.Network
import           Data.Conduit.Blaze
import           Data.ByteString (ByteString)
import           Blaze.ByteString.Builder

type MessageSource = Source (ResourceT IO) Message
type MessageSink   = Sink Message (ResourceT IO) ()
data Connection = Connection MessageSource MessageSink S.Socket

connspec = case spec "tcp://0.0.0.0:7890" of
  Just a -> a
  Nothing -> error "no way that didn't work"

--  (MonadIO m, MonadUnsafeIO m, MonadThrow m) => 
client :: ConnSpec -> Identity -> IO Connection
client connspec@(servaddr,servport,socktype) id = do
  -- Setup the outgoing socket using basic network commands
  addrinfos <- S.getAddrInfo (Just S.defaultHints) (Just servaddr) (Just servport)
  let servinfo = head addrinfos
  sock <- S.socket (S.addrFamily servinfo) socktype S.defaultProtocol
  S.connect sock (S.addrAddress servinfo)

  -- Setup the outgoing data stream
  let ungreeted_sink = builderToByteString =$ sinkSocket sock
  handshake <- runResourceT $ (identitySource Anonymous) $$ ungreeted_sink
  let greeted_sink   = messageToBuilderConduit =$ ungreeted_sink

  -- Setup the incoming data stream
  let ungreeted_unid_source = sourceSocket sock
    -- This is a strict interpretation of how the other ZMQ respondent will behave. Pattern match failure if they're deviant!
  let get_identity = ungreeted_unid_source $$ (sinkParser getMessage)
  (Message (their_ident:[])) <- runResourceT $ get_identity
  putStrLn $ "Their identity: " ++ show their_ident
  let ungreeted_source = ungreeted_unid_source

  let greeted_message_source   = ungreeted_source $= messageParserConduit

  return $ Connection greeted_message_source greeted_sink sock

messageParserConduit :: Pipe ByteString Message (ResourceT IO) ()
messageParserConduit  = sequence $ sinkParser getMessage

identitySource :: Identity -> Source (ResourceT IO) Builder
identitySource identity = yield $ buildIdentityMessage identity

messageToBuilderConduit :: Monad m => Conduit Message m Builder
messageToBuilderConduit = CL.map buildMessage

recvMessage     (Connection src _ _)      = runResourceT $ src $$ await
sendMessage     (Connection _ snk _)  msg = runResourceT $ yield msg $$ snk
closeConnection (Connection _ _ sock)     = S.sClose sock

do_connect  = do
  putStrLn "connecting... "
  connection <- client connspec Anonymous
  putStrLn "connected!"
  sendMessage connection (Message ["hi"])
  sendMessage connection (Message ["hi there"])
  sendMessage connection (Message ["hi there mr conduit"])
  _recv <- recvMessage connection
  case _recv of
    Just (Message xs) -> putStrLn $ show xs
    Nothing           -> putStrLn "failed to pull a message"
  closeConnection connection
