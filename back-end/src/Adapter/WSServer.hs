{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.WSServer
  ( webSocketServer,
  )
where

import Control.Exception (SomeException, catch)
import Control.Monad (forever, when)
import Control.Monad.Cont (MonadIO (liftIO))
import Data.Text (Text)
import qualified Data.Text as Text
import Domain.Connection (ConnId, ConnState (..), ConnStatus (..), ConnectionsRepo (..))
import Domain.Types
import Domain.User (UsersRepo (..))
import qualified Network.WebSockets as WS
import Params (_DEBUG_MODE)
import Text.Printf (printf)
import Utils.Utils

type WSSApp m = (MonadIO m, UsersRepo m, ConnectionsRepo m)

webSocketServer :: WSSApp m => PingTime -> WS.PendingConnection -> m ()
webSocketServer pingTime pending = do
  conn <- liftIO $ WS.acceptRequest pending
  (connId, connState) <- handshake conn

  liftIO $ logger LgInfo $ printf "Connected %s" (show connId)

  -- for debug
  when _DEBUG_MODE $ do
    -- connState <- lookupConnState (getConnRepo wss) idConn
    liftIO $ logger LgDebug $ show connState

  listener <- threadMessageListener connId connState
  liftIO $ WS.withPingThread conn pingTime (pure ()) $ do
    catch
      (pure listener)
      (\(e :: SomeException) -> putStrLn ("WebSocket thread error: " ++ show e) >> disconnect connId)

  pure ()
  where
    disconnect connId = do
      --   removeConn (getConnRepo wss) idConn -- TODO
      logger LgInfo $ show connId ++ " disconnected"

handshake :: WSSApp m => WS.Connection -> m (ConnId, ConnState)
handshake conn = do
  -- TODO it's just a mock, implement some normal handshake
  userIdAnon <- addAnonUser
  addConn conn (Left userIdAnon) NormalConnection

threadMessageListener :: WSSApp m => ConnId -> ConnState -> m ()
threadMessageListener connId ConnState {connStateWSConnection} = forever $ do
  (msg :: Text) <- liftIO $ WS.receiveData connStateWSConnection
  liftIO $ logger LgInfo $ "RECIEVE #(" <> show connId <> "): " <> Text.unpack msg
  pure ()
