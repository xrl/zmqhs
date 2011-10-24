{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Data.ByteString as B (pack)
import qualified Data.Attoparsec as AP
import qualified ZMQHS.Frame     as ZF

test_one_complete   = B.pack [1,4,65]
test_two_incomplete = B.pack [1,0]
test_two_rest       = B.pack [66]

main = do
    AP.parseTest ZF.parser test_one_complete
    AP.parseTest ZF.parser test_two_incomplete
    case (AP.parse ZF.parser test_two_incomplete) of
        AP.Partial cont -> case (cont test_two_rest) of
                            AP.Done _ val        -> putStrLn ("Got val: " ++ show val)
                            AP.Partial more      -> putStrLn ("Need more")
                            AP.Fail dat ctxs msg -> putStrLn (show msg)
    
    --case AP.parse ZF.parser test_data of
    --  AP.Partial moar      -> putStrLn "Need more"
    --  AP.Done bs retval    -> putStrLn $ show retval
    --  AP.Fail dat ctxs msg -> putStrLn $ show msg
    --where parser = ZF.parser