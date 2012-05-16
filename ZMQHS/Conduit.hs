{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module ZMQHS.Conduit
( module X )
where
import ZMQHS
import           Data.Attoparsec
import           Data.Conduit            as X
import           Data.Conduit.Network    as X
import           Data.Conduit.Attoparsec as X
import qualified Data.ByteString         as S

sink :: (MonadResource m, MonadThrow m) => Sink S.ByteString m Message
sink = sinkParser getMessage

source socket = sourceSocket socket

client connspec = do
  Client sock <- connect connspec Anonymous
  return sock

messages sock = do
  (source sock) $$ sink

--blah = do
--  case spec "tcp://0.0.0.0:7890" of
--    Just connspec -> go connspec
--    Nothing ->    putStrLn "eh"

--go connspec = do
--  Client sock <- connect connspec
--  stuff <- runResourceT $ messages sock
--  return stuff