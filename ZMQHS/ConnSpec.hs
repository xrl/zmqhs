module ZMQHS.ConnSpec
(
  spec,
  ConnSpec
)
where
import qualified Network.URI as U
import qualified Network.Socket as S

type ConnSpec = (S.HostName, S.ServiceName, S.SocketType)

spec :: String -> Maybe ConnSpec
spec strspec = do
  uri <- U.parseURI strspec
  specFromURI uri

specFromURI :: U.URI -> Maybe ConnSpec
specFromURI uri = do
      auth <- U.uriAuthority uri
      sock <- socktype
      return (U.uriRegName auth, port auth, sock)
      where socktype
              | U.uriScheme uri == "tcp:" = Just S.Stream
              | otherwise                 = Nothing
            port = tail . U.uriPort

-- I need this so often I might as well put it here
connspec :: ConnSpec
connspec =
  case spec "tcp://0.0.0.0:7890" of
    Just specy -> specy
    Nothing -> error "never gonna happen!"