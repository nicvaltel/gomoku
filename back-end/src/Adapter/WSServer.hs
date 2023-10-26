{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Adapter.WSServer
  ( webSocketServer,
  )
where

-- import Control.Exception (SomeException(..), catch, Exception (toException))
import Control.Monad.Catch(SomeException, catch, MonadCatch)
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

type WSSApp m = (MonadIO m, UsersRepo m, ConnectionsRepo m, MonadCatch m)

webSocketServer :: WSSApp m => PingTime -> WS.PendingConnection -> m ()
webSocketServer pingTime pending = do
  conn <- liftIO $ WS.acceptRequest pending
  (connId, connState) <- handshake conn

  liftIO $ logger LgInfo $ printf "Connected %s" (show connId)

  when _DEBUG_MODE $ do
    liftIO $ logger LgDebug $ show connState

  threadMessageListenerIO <- threadMessageListener connId connState
  liftIO $ WS.withPingThread conn pingTime (pure ()) $ pure threadMessageListenerIO


handshake :: WSSApp m => WS.Connection -> m (ConnId, ConnState)
handshake conn = do
  -- TODO it's just a mock, implement some normal handshake
  userIdAnon <- addAnonUser
  addConn conn (Left userIdAnon) NormalConnection

threadMessageListener :: WSSApp m => ConnId -> ConnState -> m ()
threadMessageListener connId connState = 
  catch
    (messageListener connId connState)
    (\(e :: SomeException) -> do
      liftIO $ putStrLn ("WebSocket thread error: " ++ show e) 
      disconnect)
  where
    disconnect = do
      deleteConn connId
      liftIO $ logger LgInfo $ show connId ++ " disconnected"

messageListener :: WSSApp m => ConnId -> ConnState -> m ()
messageListener connId ConnState {connStateWSConnection} = forever $ do
  (msg :: Text) <- liftIO $ WS.receiveData connStateWSConnection
  liftIO $ logger LgInfo $ "RECIEVE #(" <> show connId <> "): " <> Text.unpack msg

