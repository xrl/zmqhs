import           Test.HUnit
import           Test.Hspec

import Control.Exception (fromException)
import Control.Monad.Trans

import qualified ZMQHS as Z
import qualified Data.Attoparsec.ByteString as AP

import qualified Data.ByteString as B

test_one_complete      = B.pack [1,1,65]
test_two_incomplete    = B.pack [1,0]
test_two_rest          = B.pack [67]
test_three_complete    = B.pack [0xFF,1,0,0,0,0,0,0,0,1,67]

first_stage_handshake  = B.pack [0x01, 0x7E]
second_stage_handshake = B.pack [0x1d, 0x7e, 0x41, 0x53, 0x44, 0x46,
                                 0x41, 0x53, 0x44, 0x46, 0x41, 0x53,
                                 0x44, 0x46, 0x41, 0x53, 0x44, 0x46,
                                 0x41, 0x53, 0x44, 0x46, 0x41, 0x53,
                                 0x44, 0x46, 0x41, 0x53, 0x44, 0x46]

main = do
  Z.debugIt undefined
-- hspec spec
-- https://github.com/snoyberg/conduit/blob/master/attoparsec-conduit/test/main.hs
spec = do
  describe "parsing frames" $ do
    context "a minimal frame" $ do
      describe "the parser" $ do
        it "reads all the bytes" $ pending
          --case (AP.parse Z.frameParser (B.pack [65])) of
          --  (AP.Done _ (Z.FinalFrame (payload))) -> True
          --  otherwise -> False