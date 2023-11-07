{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.MessagesOutput where

import qualified Data.ByteString.Char8 as BSC8
import Domain.Connection (ConnId (..))
import Domain.Room (RoomId (..), RoomStatus (..))
import Domain.Types (MessageStr)
import Domain.User (RegStatus (Anonim, Registered), UserId (..))
import qualified Network.WebSockets as WS
import Utils.Utils

data WebSocketOutputMessage
  = ResendIncorrectOutMsg
  | GameRoomOutMsg RoomMsg
  | LoginLogoutOutMsg LoginLogoutMsg
  deriving (Show)

data RoomMsg
  = GameRoomCreated (RoomId 'LobbyRoom)
  | GameRoomIsAlreadyActive (RoomId 'ActiveRoom)
  deriving (Show)

data LoginLogoutMsg
  = AskForExistingUser
  | RegisterError
  | RegisteredSuccessfully (UserId 'Registered) ConnId
  | LoginError
  | LoginSuccessfully (UserId 'Registered) ConnId
  | LogoutSuccessfully (UserId 'Anonim) ConnId
  | NewAnonUser (UserId 'Anonim) ConnId
  deriving (Show)

toWebSocketOutputMessage :: WebSocketOutputMessage -> MessageStr
toWebSocketOutputMessage ResendIncorrectOutMsg = "ResendIncorrectOutMsg"
toWebSocketOutputMessage (GameRoomOutMsg roomMsg) = "GameRoomOutMsg;" <> toRoomMsg roomMsg
toWebSocketOutputMessage (LoginLogoutOutMsg loginLogoutMsg) = "GameRoomOutMsg;" <> toLoginLogoutOutMsg loginLogoutMsg

toLoginLogoutOutMsg :: LoginLogoutMsg -> MessageStr
toLoginLogoutOutMsg AskForExistingUser = "AskForExistingUser"
toLoginLogoutOutMsg RegisterError = "RegisterError"
toLoginLogoutOutMsg (RegisteredSuccessfully (UserId userId) (ConnId connId)) = "RegisteredSuccessfully;" <> BSC8.pack (show userId) <> ";" <> BSC8.pack (show connId)
toLoginLogoutOutMsg LoginError = "LoginError"
toLoginLogoutOutMsg (LoginSuccessfully (UserId userId) (ConnId connId)) = "LoginSuccessfully;" <> BSC8.pack (show userId) <> ";" <> BSC8.pack (show connId)
toLoginLogoutOutMsg (LogoutSuccessfully (UserId userId) (ConnId connId)) = "LogoutSuccessfully;" <> BSC8.pack (show userId) <> ";" <> BSC8.pack (show connId)
toLoginLogoutOutMsg (NewAnonUser (UserId userId) (ConnId connId)) = "NewAnonUser;" <> BSC8.pack (show userId) <> ";" <> BSC8.pack (show connId)

toRoomMsg :: RoomMsg -> MessageStr
toRoomMsg (GameRoomCreated (RoomId roomId)) = "GameRoomCreated;" <> BSC8.pack (show roomId)
toRoomMsg (GameRoomIsAlreadyActive (RoomId roomId)) = "GameRoomIsAlreadyActive;" <> BSC8.pack (show roomId)

sendWebSocketOutMsg :: String -> WS.Connection -> WebSocketOutputMessage -> IO ()
sendWebSocketOutMsg logPrefix conn msg = do
  let jsonMsg = toWebSocketOutputMessage msg
  logger LgMessage $ logPrefix ++ show jsonMsg
  WS.sendTextData conn jsonMsg