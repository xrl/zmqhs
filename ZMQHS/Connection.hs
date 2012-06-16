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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Network.Socket as S
import qualified Data.Binary.Put as P
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Network
import           Data.ByteString (ByteString)

data Connection m = Connection (IO (m Message)) (IO (Sink ByteString IO ()))

--client :: (IO (Source IO ByteString)) -> IO Message
--client source_promise = do
--  source <- source_promise
--  ($$) source (sinkParser getMessage)

--sink :: ConnSpec -> Identity -> IO (Sink ByteString IO ())
--sink connspec@(servaddr,servport,socktype) id = do
--  addrinfos <- S.getAddrInfo (Just S.defaultHints) (Just servaddr) (Just servport)
--  let servaddrinfo = head addrinfos
--  sock <- S.socket (S.addrFamily servaddrinfo) socktype S.defaultProtocol
--  S.bindSocket sock (S.addrAddress servaddrinfo)
--  S.listen sock 1
--  return $ sinkSocket sock

connspec = case spec "tcp://0.0.0.0:7890" of
  Just a -> a
  Nothing -> error "no way that didn't work"

client :: ConnSpec -> Identity -> IO ( (Source IO ByteString), (Sink ByteString a IO))
client connspec@(servaddr,servport,socktype) id = do
  addrinfos <- S.getAddrInfo (Just S.defaultHints) (Just servaddr) (Just servport)
  let servinfo = head addrinfos
  sock <- S.socket (S.addrFamily servinfo) socktype S.defaultProtocol
  S.connect sock (S.addrAddress servinfo)
  return $ (sourceSocket sock,sinkSocket sock)

handshake :: MonadResource m => Sink ByteString m a -> Sink ByteString m a
handshake NeedInput{} =
  let action = liftIO $ putStrLn "hi!"
  in PipeM action (lift action)


do_connect = do
  (src,snk) <- client connspec Anonymous
  putStrLn "connecting... "
  src $$+ sinkParser getMessage
  putStrLn "connected!"