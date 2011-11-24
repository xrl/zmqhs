#!/usr/bin/env runhaskell

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P

some_data = B.pack [65,67]

getter = do
  byte_0 <- G.getWord8
  byte_1 <- G.getWord8
  return (byte_0,byte_1)

putter (byte_0, byte_1) = do
  P.putWord8 byte_0
  P.putWord8 byte_1

main = do
  case (G.runGet getter some_data) of
    (one,two) -> putStrLn (show one ++ " " ++ show two) >>
                 putStrLn "read it!" >>
                 case (P.runPut (putter (one,two))) of
                   a_str -> B.putStrLn some_data >>
                            B.putStrLn a_str