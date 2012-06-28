#!/usr/bin/env runhaskell -i..

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

import System.Console.CmdArgs (cmdArgs, Data, Typeable, help, enum, (&=), typ, program, summary)

import qualified ZMQHS as Z

import Prelude hiding (catch)
import Control.Exception (bracket, catch)
import Control.Monad (forever)

import Data.ByteString.Char8 (pack)

import Blaster.Application

data Mode = Pub | Sub deriving (Typeable, Data, Show)
data CmdOp = CmdOp {mode_        :: Mode,
                    target_      :: String,
                    identity_    :: String,
                    payload_     :: String,
                    repetitions_ :: Int}
     deriving (Typeable, Data, Show)

data Op = Op {mode        :: Mode,
              target      :: String,
              target_spec :: Maybe Z.ConnSpec,
              identity    :: String,
              payload     :: String,
              repetitions :: Int}
     deriving (Show)

operation :: CmdOp
operation = CmdOp {mode_        = enum [Pub &= help "Pub",
                                        Sub &= help "Sub"],
                   target_      = "" &= typ "URI" &= help "Target URI spec",
                   identity_    = "anonymous" &= typ "ID" &= help "Use name on all messages",
                   payload_     = "TESTPAYLOAD" &= typ "BYTES" &= help "Message payload",
                   repetitions_ = 1 &= typ "REPEAT" &= help "Repititions"}
                &= program "blaster"
                &= summary "One stop shop for poking around your ZMQ network"
                -- &= verbosity

main :: IO ()
main = do
  raw_args <- cmdArgs operation
  let args = op_from_cmd_op raw_args
  case (target_spec args) of
    Just _  -> exec args
    Nothing -> putStrLn "Target spec must be a valid spec"
  return ()

op_from_cmd_op :: CmdOp -> Op
op_from_cmd_op CmdOp{mode_=m,target_=t,identity_=i,payload_=p,repetitions_=r} =
  Op {mode        = m,
      target      = t,
      target_spec = Z.spec t,
      identity    = i,
      payload     = p,
      repetitions = r}

to_ident :: String -> Z.Identity
to_ident  "anonymous" = Z.Anonymous
to_ident  name        = Z.Named (pack name)


--serv <- Z.server targ $ to_ident identstr
--conn <- Z.accept serv
--_ <- writeMessages conn 1 outgoing
--Z.closeConn conn
exec :: Op -> IO ()
exec Op {mode=Sub, target_spec=Just targ, payload=outgoing, identity=identstr} = do
  forever $
    catch
      (bracket (Z.startServer targ $ to_ident identstr)
               (Z.stopServer)
               (\serv -> (do
                   bracket (putStrLn "accepting..." >> Z.accept serv)
                           (Z.closeConn)
                           (\conn -> writeMessages conn 1 outgoing)
                         )
               ))
      (\err -> putStrLn $ "Exception caught: " ++ show (err :: IOError))
exec Op {mode=Pub, target_spec=Just spec, payload=outgoing, identity=identstr,repetitions=rep} = do
  conn <- Z.client spec (to_ident identstr)
  asyncws <- mapM (\_ -> spawnWriter conn rep outgoing) [1..10] 
  asyncrs <- mapM (\_ -> spawnReader conn)              [1..10] 

  mapM_ Z.wait asyncws
  mapM_ Z.wait asyncrs

  return ()
exec Op {target_spec=Nothing} = do
  return ()

--foldr (>>) (return ()) (take rep $ repeat ()