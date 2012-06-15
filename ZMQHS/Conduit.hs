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

conduitClient str = do
  case spec str of
    Just connspec -> do
      conn <- connection connspec Anonymous
      putStrLn "hi"
    Nothing -> do
      putStrLn "could not make a conn spec"