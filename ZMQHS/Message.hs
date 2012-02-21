module ZMQHS.Message
where
import qualified Data.ByteString      as BS

data Identity = Anonymous
              | Named BS.ByteString -- can't be empty
    deriving(Show)
data Message = Message Identity [BS.ByteString]
    deriving (Show)