module Main (main)
where

import           Debug.Trace (trace)

import           Test.Hspec

import qualified ZMQHS as Z
import qualified Data.Attoparsec.ByteString as AP

import qualified Data.ByteString as B

--first_stage_handshake  = B.pack [0x01, 0x7E]
--second_stage_handshake = B.pack [0x1d, 0x7e, 0x41, 0x53, 0x44, 0x46,
--                                 0x41, 0x53, 0x44, 0x46, 0x41, 0x53,
--                                 0x44, 0x46, 0x41, 0x53, 0x44, 0x46,
--                                 0x41, 0x53, 0x44, 0x46, 0x41, 0x53,
--                                 0x44, 0x46, 0x41, 0x53, 0x44, 0x46]

examples :: [(String, B.ByteString, Z.Frame)]
examples = [("complete with payload 'A'",   B.pack [2,0,65],    Z.FinalFrame $ B.pack [65]),
            ("incomplete with payload 'A'", B.pack [2,1,65],    Z.MoreFrame  $ B.pack [65]),
            ("first handshake",             B.pack [0x01,0x7E], Z.FinalFrame $ B.pack []  )]

main :: IO ()
main = do
  hspec $ describe "parsing frames" $ do
    context "the parser" $ do
      mapM_ itRunner examples

itRunner :: (String,B.ByteString,Z.Frame) -> Spec
itRunner (msg,frame,expected) = it msg (frameExample frame expected)

frameExample :: B.ByteString -> Z.Frame -> Bool
frameExample bytes expected = case AP.parse Z.frameParser bytes of
    AP.Done leftover res -> res == expected && B.null leftover
    _ -> False

--it msg $ case AP.parse Z.frameParser frame == expected of
--          True  -> True
--          False -> trace ("Failed on " ++ show frame) False

-- https://github.com/snoyberg/conduit/blob/master/attoparsec-conduit/test/main.hs


--case (AP.parse Z.frameParser test_one_complete) of
--  (AP.Done _ (Z.FinalFrame (payload))) -> True
--  (AP.Partial _) -> True
--  (AP.Fail _ _ _) -> False
--  otherwise -> trace (show otherwise) (False)