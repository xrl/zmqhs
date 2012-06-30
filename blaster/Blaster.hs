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
                                        Sub &= help "Sub",
                                        Interactive &= help "Interactive"],
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
  exec args
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
exec Op {mode=Interactive} = do
  run Nothing interactive
exec Op {target_spec=Nothing} = do
  return ()

readAllMessages :: Z.Connection -> IO ()
readAllMessages conn = do
  foldr (>>) (return ()) $ repeat $ do
    msg <- Z.recvMessage conn
    case msg of
      Just _  -> putChar '.'
      Nothing -> putStrLn "got nothing?"

writeMessages :: Z.Connection -> Int -> String -> IO ()
writeMessages conn rep outgoing = do
  foldr (>>) (return ()) (take rep $ repeat (Z.sendMessage conn (Z.Message [pack outgoing])))

--myForkIO :: IO () -> IO (MVar ())
--myForkIO io = do
--  mvar <- newEmptyMVar
--  _ <- forkIO (io `finally` putMVar mvar ())
--  return mvar  putStrLn "Sorry, please enter a valid target specification (e.g., \"tcp://localhost:7890\""


interactive :: StateIO (Maybe Z.Connection) ()
interactive = do line <- io getLine
                 let cmd = words line
                 case cmd of
                      ("quit":_) -> return ()
                      otherwise  -> do interprete $ words line
                                       interactive

interprete :: [String] -> StateIO (Maybe Z.Connection) ()
interprete (cmd:xs)  
    | cmd == "connect"    = interpreteConnect xs     
    | cmd == "disconnect" = interpreteDisconnect xs
    | cmd == "send"       = interpreteSend xs
    | cmd == "recv"       = interpreteRecv xs
    | cmd == "help"       = io $ putStrLn printHelp
    | otherwise           = case reads cmd of 
                                 [(n, "")] -> sequence_ $ take n $ repeat $ interprete (xs) 
                                 _         -> io $ putStrLn "Command error"

interprete _ = io $ putStrLn ""


printHelp :: String
printHelp = "Usage :\n\
             \\tconnect URI identity\n\
             \\tdisconnect\n\
             \\t[repeat] send frame1 frame2 frame3...\n\
             \\t[repeat] recv"

interpreteConnect :: [String] -> StateIO (Maybe Z.Connection) ()
interpreteConnect (target:identity:[]) = case Z.spec target of
                                              Just target_spec -> do io $ putStrLn $ "Connecting " ++ target ++ "..."
                                                                     conn <- io $ Z.client target_spec (to_ident identity)
                                                                     setState $ Just conn
                                                                     io $ putStrLn $ "Connected !"
                                              otherwise -> io $ putStrLn $ "Bad target :" ++ target
interpreteConnect _                     = io $ putStrLn "Usage: connect <target> <identity>"

interpreteDisconnect :: [String] -> StateIO (Maybe Z.Connection) ()
interpreteDisconnect [] =  do connection <- getState 
                              case connection of
                                   Just conn -> do io $ Z.closeConnection conn
                                                   setState Nothing
                                                   io $ putStrLn "Disconnected"
                                   Nothing   -> return ()
    

interpreteSend :: [String] -> StateIO (Maybe Z.Connection) ()
interpreteSend payload = do connection <- getState
                            case connection of
                                 Just conn -> io $ (Z.sendMessage conn (Z.Message (map pack payload))) >> (putStrLn $ "Sent message " ++ (unwords payload))
                                 otherwise -> io $ putStrLn "Need to be connected to send a message"


interpreteRecv :: [String] -> StateIO (Maybe Z.Connection) ()
interpreteRecv _ = do conn <- getState
                      case conn of
                           Just conn -> io $Z.recvMessage conn >>= \x -> case x of
                                                                        Just (Z.Message payload) -> putStrLn $ "Received " ++ (show $ B.concat payload)
                                                                        otherwise                -> return ()
                           otherwise -> io $ putStrLn "Need to be connected to receive messages"

mapMaybeStateIO :: (a -> StateIO b c) -> Maybe a -> StateIO b ()
mapMaybeStateIO _ Nothing  = return ()
mapMaybeStateIO f (Just a) = f a >> return ()

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing