module Main (main) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async

import Network.QUIC.Simple qualified as QUIC
import Network.Message

import Data.IntMap qualified as IntMap
import Data.IORef (newIORef, atomicModifyIORef')

import Codec.Serialise(Serialise)

import Server.Simulation

main :: IO ()
main = do
  conns <- newTVarIO mempty
  connIds <- newIORef 0
  QUIC.runServerStateful "127.0.0.1" 2525 (setup conns connIds) (teardown conns) handler
  where
    setup conns counter conn writeQ = do
      connId <- atomicModifyIORef' counter \old -> (old + 1, old)
      me <- myThreadId >>= mkWeakThreadId
      atomically $ modifyTVar' conns $ IntMap.insert connId (me, conn)
      pure (connId, writeQ, 0 :: Int)

    teardown conns _conn (connId, _writeQ, _counter) =
      atomically $ modifyTVar' conns $ IntMap.delete connId

    handler s@(connId, writeQ, counter) msg = do
      case msg of
        (Call id Ping) -> do
          putStrLn "Server got ping"
          atomically $ writeTBQueue writeQ (Reply id Pong)
          pure ((connId, writeQ, counter + 1), Nothing)
        _ -> pure (s, Nothing)



