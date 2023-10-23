{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.MessagesOutput where

import Data.Aeson (FromJSON, ToJSON)
import Domain.Room (RoomId, RoomStatus (..))
import Domain.User (RegStatus (Anonim, Registered), UserId)
import GHC.Generics (Generic)

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
  | RegisteredSuccessfully (UserId 'Registered)
  | LoginError
  | LoginSuccessfully (UserId 'Registered)
  | LogoutSuccessfully (UserId 'Anonim)
  deriving (Show, Generic)

instance FromJSON RoomMsg

instance FromJSON LoginLogoutMsg

instance FromJSON WebSocketOutputMessage

instance ToJSON RoomMsg

instance ToJSON LoginLogoutMsg

instance ToJSON WebSocketOutputMessage
