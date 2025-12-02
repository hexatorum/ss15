module Main (main) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async

import Network.QUIC.Simple qualified as QUIC
import Network.Message

import Data.Text(Text, unpack, pack)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.IORef (newIORef, atomicModifyIORef')

import Codec.Serialise(Serialise)

import Apecs
import Game
import Game.Server.Simulation

loginInfo :: Map.Map Text Text
loginInfo = Map.fromList [("test", "test")]

main :: IO ()
main = do
  players <- newTVarIO (Map.empty :: Map.Map Text Entity)
  conns <- newTVarIO mempty
  connIds <- newIORef 0

  world <- initWorld

  let registerPlayer name = runWith world $ newEntity (Player name)

  let
    handler s@(connId, writeQ) msg = do
      case msg of
        (Call id (TryLogin name pass)) -> do
          let _str_name = unpack name
          putStrLn $ "Someone trying to login as " <> _str_name
          reply <- pure (Map.lookup name loginInfo) >>= \case
            (Just acctPass) ->
              if pass == acctPass
                then do
                  ent <- registerPlayer name
                  atomically $ modifyTVar' players \players -> Map.insert name ent players
                  putStrLn $ _str_name <> ": Login successful"
                  pure $ Just (Reply id (LoginSuccess (unEntity ent)))
                else do
                  atomically $ writeTBQueue writeQ (Reply id LoginFail)
                  putStrLn $ _str_name <> ": Login failed: invalid password"
                  error "disconnect"
                  pure Nothing
            Nothing -> do
              atomically $ writeTBQueue writeQ (Reply id LoginFail)
              putStrLn $ _str_name <> ": Login failed: account doesn't exist"
              error "disconnect"
              pure Nothing
          pure (s, reply)
        (Call id Ping) -> do
          pure (s, Just (Reply id Pong))
        _ -> pure (s, Nothing)


  QUIC.runServerStateful "127.0.0.1" 2525 (setup conns connIds) (teardown conns) handler
    where
      setup conns counter conn writeQ = do
        connId <- atomicModifyIORef' counter \old -> (old + 1, old)
        me <- myThreadId >>= mkWeakThreadId
        atomically $ modifyTVar' conns $ IntMap.insert connId (me, conn)
        pure (connId, writeQ)

      teardown conns _conn (connId, _writeQ) =
        atomically $ modifyTVar' conns $ IntMap.delete connId



