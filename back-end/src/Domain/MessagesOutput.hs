{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.MessagesOutput where

import Data.Aeson (FromJSON, ToJSON, encode)
import Domain.Room (RoomId, RoomStatus (..))
import Domain.User (RegStatus (Anonim, Registered), UserId)
import GHC.Generics (Generic)
import qualified Network.WebSockets as WS
import Utils.Utils
import Domain.Connection (ConnId)

data WebSocketOutputMessage
  = ResendIncorrectOutMsg
  | GameRoomOutMsg RoomMsg
  | LoginLogoutOutMsg LoginLogoutMsg
  deriving (Show, Generic)

data RoomMsg
  = GameRoomCreated (RoomId 'LobbyRoom)
  | GameRoomIsAlreadyActive (RoomId 'ActiveRoom)
  deriving (Show, Generic)

data LoginLogoutMsg
  = AskForExistingUser
  | RegisterError
  | RegisteredSuccessfully (UserId 'Registered) ConnId
  | LoginError
  | LoginSuccessfully (UserId 'Registered) ConnId
  | LogoutSuccessfully (UserId 'Anonim) ConnId
  | NewAnonUser (UserId 'Anonim) ConnId
  deriving (Show, Generic)

instance FromJSON RoomMsg

instance FromJSON LoginLogoutMsg

instance FromJSON WebSocketOutputMessage

instance ToJSON RoomMsg

instance ToJSON LoginLogoutMsg

instance ToJSON WebSocketOutputMessage


sendWebSocketOutMsg :: String -> WS.Connection -> WebSocketOutputMessage -> IO ()
sendWebSocketOutMsg logPrefix conn msg = do
  let jsonMsg = encode msg
  logger LgMessage $ logPrefix ++ show jsonMsg
  WS.sendTextData conn jsonMsg