{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Data.ByteString as B (pack)

test_data = B.pack [0,0,0]

main = do
    Prelude.putStrLn "hi"