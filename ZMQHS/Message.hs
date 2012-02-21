module ZMQHS.Message
where
import qualified Data.ByteString      as BS

data Identity = Anonymous
              | Named BS.ByteString -- can't be empty
    deriving(Show)
data Message = Message Identity [BS.ByteString]
    deriving (Show)

--parseIdentity :: AP.Parser Identity
--parseIdentity = do
--  len <- parseLength
--  _ <- parseFinal
--  (if len == 1
--     -- anonymous, let's record that
--     then trace "anon"  $ return Anonymous 
--     else trace "named" $ Named <$> AP.take (len - 1)) <?> "identity"