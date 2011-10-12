module ZMQHS.Frame(
    parse
)
where
import qualified Data.ByteString.Char8 as B
--import qualified Data.Binary.Strict.Get as G
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import           Data.Bits
import qualified Data.Binary.Strict.BitGet as BG

parse = do
    id <- G.getWord16be
    return id