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
where
import ZMQHS.Frame
import Control.Monad (guard)
import Control.Applicative hiding (empty)
import           Data.Word (Word8, Word64)
import qualified Data.Attoparsec as AP
import qualified Data.Attoparsec.Binary as APB
import qualified Network.Socket.ByteString.Lazy as LSB

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