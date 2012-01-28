#!/usr/bin/env runhaskell

{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Data.ByteString as B (pack)
import qualified Data.Attoparsec as AP
import qualified ZMQHS.Frame     as ZF
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P

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
    AP.parseTest ZF.parser first_stage_handshake
    AP.parseTest ZF.parser second_stage_handshake

old_main = do
    AP.parseTest ZF.parser test_one_complete
    AP.parseTest ZF.parser test_two_incomplete
    case (AP.parse ZF.parser test_two_incomplete) of
        AP.Partial cont -> case (cont test_two_rest) of
                            AP.Done _ val        -> putStrLn ("Got val: " ++ show val)
                            AP.Partial more      -> putStrLn ("Need more")
                            AP.Fail dat ctxs msg -> putStrLn (show msg)
    case (AP.parse ZF.parser test_three_complete) of
    	AP.Done _ val -> putStrLn ("Got val " ++ show val)
    	AP.Partial more -> putStrLn ("Moar plz")
    	AP.Fail dat ctxs msg -> putStrLn (show msg)

blah = do
    [ return P.putWord8 x | x <- [0..100] ]
    --mapM P.putWord8 [0..100]