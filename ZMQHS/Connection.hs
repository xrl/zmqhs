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

data Connection m = Connection (IO (m Message)) (IO (Sink ByteString IO ()))

connspec = case spec "tcp://0.0.0.0:7890" of
  Just a -> a
  Nothing -> error "no way that didn't work"

--  (MonadIO m, MonadUnsafeIO m, MonadThrow m) => 
client :: ConnSpec -> Identity -> IO ( (Source (ResourceT IO) Message), (Sink Message (ResourceT IO) ()))
client connspec@(servaddr,servport,socktype) id = do
  addrinfos <- S.getAddrInfo (Just S.defaultHints) (Just servaddr) (Just servport)
  let servinfo = head addrinfos
  sock <- S.socket (S.addrFamily servinfo) socktype S.defaultProtocol
  S.connect sock (S.addrAddress servinfo)
  let parserConduit = sequence $ sinkParser getMessage
  let source        = sourceSocket sock       $= parserConduit
  let sink          = messageToBuilderConduit =$ builderToByteString =$ sinkSocket sock
  return $ (source,sink)

handshake = undefined

messageSource :: Monad m => Message -> Source m Message
messageSource m = HaveOutput (Done Nothing ())  (return ()) m

messageToBuilderConduit :: Monad m => Conduit Message m Builder
messageToBuilderConduit = CL.map buildMessage

tell_me = ($$) (messageSource (Message Anonymous []))

do_connect = do
  putStrLn "connecting... "
  (src,snk) <- client connspec Anonymous
  runResourceT (tell_me snk)
  putStrLn "connected!"
