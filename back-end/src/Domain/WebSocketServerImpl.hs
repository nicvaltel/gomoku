{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Domain.WebSocketServerImpl where

import Domain.MessagesInput
import Domain.Connection
import Data.Text (Text)
import Domain.Types
import qualified Network.WebSockets as WS
import Domain.User (UsersRepo(..))
import Control.Exception (SomeException)
import Utils.Utils
import Text.Printf (printf)
import Control.Monad.Cont (MonadIO(liftIO))
import Control.Monad (when)
import Params ( _DEBUG_MODE )
import Control.Monad.Catch (catch)
import Domain.WebSocketServer


-- type ConnectionsIntMap = RepoIntMap DC.ConnState

-- type ConnsDB = TVar ConnectionsIntMap

type WSSApp = (UsersRepo IO, ConnectionsRepo IO, WebSocketServer IO)


run__ :: Host -> Port -> IO ()
run__ host port = do
  WS.runServer host port (\pending -> pure ())



runWebSocketServer :: WSSApp => Host -> Port -> PingTime -> IO ()
runWebSocketServer host port pingTime = do
        WS.runServer host port $ \pending -> webSocketServer pingTime pending


webSocketServer :: PingTime -> WS.PendingConnection -> IO ()
webSocketServer pingTime pending = do
  conn <- WS.acceptRequest pending 
  pure ()

  -- WS.withPingThread conn pingTime (pure ()) $ do
  --   catch
  --     (wsThreadMessageListener wss conn idConn)
  --     (\(e :: SomeException) -> (putStrLn $ "WebSocket thread error: " ++ show e) >> disconnect idConn)
  -- pure ()
  -- where
  --   disconnect idConn = do
  --     -- removeConn (getConnRepo wss) idConn
  --     logger LgInfo $ show idConn ++ " disconnected"





-- runWebSocketServer :: Host -> Port -> PingTime -> IO ()
-- runWebSocketServer host port pingTime = do
--         WS.runServer host port $ webSocketServer wss pingTime

-- webSocketServer :: (UsersRepo m, ConnectionsRepo m, WebSocketServer m, MonadIO m) => PingTime -> WS.PendingConnection -> m ()
-- webSocketServer pingTime = \pending -> do
--   conn <- liftIO $ WS.acceptRequest pending
--   (connState, connId) <- handshake conn
--   liftIO $ logger LgInfo $ printf "connected user=%s conn=%s" (show $ connStateUserId connState) (show connId)

--   when _DEBUG_MODE $ liftIO $ do
--     pure ()
--     -- connState <- lookupConnState (getConnRepo wss) idConn
--     -- logger LgDebug $ show connState

--   WS.withPingThread conn pingTime (pure ()) $ do
--     catch
--       (wsThreadMessageListener wss conn idConn)
--       (\(e :: SomeException) -> (putStrLn $ "WebSocket thread error: " ++ show e) >> disconnect idConn)
--   pure ()
--   where
--     disconnect idConn = do
--     --   removeConn (getConnRepo wss) idConn
--       logger LgInfo $ show idConn ++ " disconnected"