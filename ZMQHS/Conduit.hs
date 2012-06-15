{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module ZMQHS.Conduit

where
import ZMQHS.Message
import ZMQHS.ConnSpec
import ZMQHS.Connection
import           Data.Attoparsec
import           Data.Conduit            as X
import           Data.Conduit.Network    as X
import           Data.Conduit.Attoparsec as X
import qualified Data.ByteString         as S

--sink :: Sink S.ByteString IO Message
--sink = sinkParser getMessage

conduitClient str = do
  case spec str of
    Just connspec -> do
      conn <- connection connspec Anonymous
      putStrLn "hi"
    Nothing -> do
      putStrLn "could not make a conn spec"

-- source socket = sourceSocket socket

--client connspec = do
--  sock <- connect connspec Anonymous
--  return sock

--client :: Connection -> IO Message
--client_conduit Client sock = do
--  (source sock) $$ (sink)


--blah = do  
--  case spec "tcp://0.0.0.0:7890" of
--    Just connspec -> go connspec
--    Nothing ->    putStrLn "eh"

--go connspec = do
--  Client sock <- connect connspec
--  stuff <- runResourceT $ messages sock
--  return stuff