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
import Control.Monad (guard)
import Control.Applicative hiding (empty)
import           Data.Word (Word8, Word64)
import qualified Data.Attoparsec as AP
import qualified Data.Attoparsec.Binary as APB
import Debug.Trace

parser = do
    first <- AP.anyWord8
    second <- AP.anyWord8
    case first of
        0x01 -> case second of
                    