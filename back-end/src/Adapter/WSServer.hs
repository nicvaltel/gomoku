{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.WSServer
  ( webSocketServer,
  )
where

import Control.Monad (forever, when)
import Control.Monad.Catch (MonadCatch, SomeException, catch)
import Control.Monad.Cont (MonadIO (liftIO))
import Data.Text (Text)
import qualified Data.Text as Text
import Domain.Connection (ConnId (..), ConnState (..), ConnStatus (..), ConnectionsRepo (..))
import Domain.MessagesInput
import Domain.MessagesOutput
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
  sendOutMsg "" conn (LoginLogoutOutMsg AskForExistingUser)
  inMsg <- receiveInMsg "" conn
  case inMsg of
    HandshakeInMsg (ExistingAnonConn connId uId) -> (liftIO $ putStrLn "TODO remove it -- ExistingAnonConn") >> oldAnonUser connId uId
    HandshakeInMsg (ExistingRegisteredUserAndConn connId uId pass) -> (liftIO $ putStrLn "TODO remove it -- ExistingRegisteredUserAndConn") >> regUserOldConn connId uId pass
    HandshakeInMsg (ExistingRegisteredUserNewConn uId pass) -> (liftIO $ putStrLn "TODO remove it -- ExistingRegisteredUserNewConn") >> regUserNewConn uId pass
    _ -> newAnonUser
  where
    regUserOldConn connId uId pass = do
      checkRes <- checkPassword uId pass
      if checkRes
        then do
          mbConnState <- findConnById connId
          case mbConnState of
            Just connState@ConnState {connStateUserId = Right userId} | userId == uId -> do
              sendOutMsg connId conn (LoginLogoutOutMsg $ LoginSuccessfully uId connId)
              updateConn connId connState {connStatus = NormalConnection}
            _ -> newConnForCheckedRedUser uId
        else do
          sendOutMsg connId conn (LoginLogoutOutMsg RegisterError)
          newAnonUser

    regUserNewConn uId pass = do
      checkRes <- checkPassword uId pass
      if checkRes
        then newConnForCheckedRedUser uId
        else do
          sendOutMsg "" conn (LoginLogoutOutMsg LoginError)
          newAnonUser

    newConnForCheckedRedUser uId = do
      connResult@(connId, _) <- addConn conn (Right uId) NormalConnection
      sendOutMsg connId conn (LoginLogoutOutMsg $ LoginSuccessfully uId connId)
      pure connResult

    oldAnonUser connId uId = do
      mbConnState <- findConnById connId
      case mbConnState of
        Just connState@ConnState {connStateUserId = Left userId} | userId == uId -> do
          sendOutMsg connId conn (LoginLogoutOutMsg $ NewAnonUser userId connId)
          updateConn connId connState {connStatus = NormalConnection}
        _ -> newAnonUser

    newAnonUser = do
      userIdAnon <- addAnonUser
      connResult@(connId, _) <- addConn conn (Left userIdAnon) NormalConnection
      sendOutMsg connId conn (LoginLogoutOutMsg $ NewAnonUser userIdAnon connId)
      pure connResult

threadMessageListener :: WSSApp m => ConnId -> ConnState -> m ()
threadMessageListener connId connState =
  catch
    (messageListener connId connState)
    ( \(e :: SomeException) -> do
        liftIO $ putStrLn ("WebSocket thread error: " ++ show e)
        disconnect
    )
  where
    disconnect = do
      deleteConn connId
      liftIO $ logger LgInfo $ show connId ++ " disconnected"

messageListener :: WSSApp m => ConnId -> ConnState -> m ()
messageListener connId ConnState {connStateWSConnection} = forever $ do
  (msg :: Text) <- liftIO $ WS.receiveData connStateWSConnection
  liftIO $ logger LgInfo $ "RECIEVE #(" <> show connId <> "): " <> Text.unpack msg

sendOutMsg :: WSSApp m => Show prefix => prefix -> WS.Connection -> WebSocketOutputMessage -> m ()
sendOutMsg prefix conn msg =
  let prefixStr = case show prefix of
        "\"\"" -> ""
        other -> other
   in liftIO $ sendWebSocketOutMsg ("SEND #" ++ prefixStr ++ "#: ") conn msg

receiveInMsg :: WSSApp m => Show prefix => prefix -> WS.Connection -> m WebSocketInputMessage
receiveInMsg prefix conn =
  let prefixStr = case show prefix of
        "\"\"" -> ""
        other -> other
   in liftIO $ receiveWebSocketInMsg ("RECIEVE #" ++ prefixStr ++ "#: ") conn