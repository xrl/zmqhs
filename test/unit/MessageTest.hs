module Main (main)
where

-- nice example, using many of the same libraries, for writing specs
-- https://github.com/snoyberg/conduit/blob/master/attoparsec-conduit/test/main.hs

-- import           Debug.Trace (trace)

import           Test.Hspec (hspec, describe, it, shouldBe, shouldSatisfy, Spec, Expectation)
import           Test.HUnit (assertFailure)

import qualified ZMQHS as Z
import qualified Data.Attoparsec.ByteString as AP

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = do
  hspec $ (describe "complete frame parsing" $ do
            mapM_ completeFrameSpec completeParses)
          >>
          (describe "messages" $ do
            mapM_ messageSpec completeMessages)

--------------------------------
-- MESSAGE SPECS
--------------------------------
messageSpec :: (String,[B.ByteString],Z.Message) -> Spec
messageSpec (msg,frames,expected) = it msg (messageExample frames expected)

messageExample :: [B.ByteString] -> Z.Message -> Expectation
messageExample frames expected = case AP.parse Z.getMessage (B.concat frames) of
  AP.Done leftover res -> (res `shouldBe` expected) >> (leftover `shouldSatisfy` B.null)
  AP.Partial _         -> assertFailure "should not get a partial result"
  AP.Fail _ _ _        -> assertFailure "should not get a failure from parsing"

completeMessages :: [(String,[B.ByteString],Z.Message)]
completeMessages =  [("one part", [B.pack [2,0,65]],                 Z.Message [B8.pack "A"]),
                     ("two part", [B.pack [2,1,65],B.pack [2,0,66]], Z.Message [B8.pack "A",B8.pack "B"])]

--------------------------------
-- FRAME SPECS
--------------------------------
completeFrameSpec :: (String,B.ByteString,Z.Frame) -> Spec
completeFrameSpec (msg,frame,expected) = it msg (frameExample frame expected)

frameExample :: B.ByteString -> Z.Frame -> Expectation
frameExample bytes expected = case AP.parse Z.frameParser bytes of
    AP.Done leftover res -> (res `shouldBe` expected) >> (leftover `shouldSatisfy` B.null)
    AP.Partial _ -> assertFailure "should not get a partial result"
    AP.Fail _ _ _ -> assertFailure "should not get a failure from parsing"

completeParses :: [(String, B.ByteString, Z.Frame)]
completeParses = [("complete   with payload",  B.pack [2,0,65],      Z.FinalFrame $ B8.pack "A"),
                  ("incomplete with payload",  B.pack [2,1,65],      Z.MoreFrame  $ B8.pack "A"),
                  ("handshake  w/o  identity", B.pack [0x01,0x7E],   Z.FinalFrame $ B8.pack "" ),
                  ("handshake  w/   identity", handshake_w_identity, Z.FinalFrame $ B8.pack "ASDFASDFASDFASDFASDFASDFASDF")]
                where handshake_w_identity = B8.concat [B.pack [29,0x7E] , B8.pack "ASDFASDFASDFASDFASDFASDFASDF"]