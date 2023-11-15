module OutputMessages where

import Prelude ((<>),($), (<$>))
import Data.Array (index, drop)
import Types
import Data.String (split, Pattern(..))
import Data.Maybe (Maybe(..))

data WebSocketOutputMessage
  = ResendIncorrectOutMsg
  | GameRoomOutMsg RoomMsg
  | LoginLogoutOutMsg LoginLogoutMsg

data RoomMsg
  = GameRoomCreated RoomId
  | GameRoomIsAlreadyActive RoomId

data LoginLogoutMsg
  = AskForExistingUser
  | RegisterError
  | RegisteredSuccessfully UserId ConnId
  | LoginError
  | LoginSuccessfully UserId ConnId
  | LogoutSuccessfully UserId  ConnId
  | NewAnonUser UserId ConnId Password
  | OldAnonUser UserId ConnId


-- encodeWebSocketOutputMessage :: WebSocketOutputMessage -> String
-- encodeWebSocketOutputMessage ResendIncorrectOutMsg = "ResendIncorrect"
-- encodeWebSocketOutputMessage (GameRoomOutMsg roomMsg) = "GameRoom;" <> encodeRoomMsg roomMsg
-- encodeWebSocketOutputMessage (LoginLogoutOutMsg loginLogoutMsg) = "LoginLogout;" <> encodeLoginLogoutOutMsg loginLogoutMsg

-- encodeLoginLogoutOutMsg :: LoginLogoutMsg -> String
-- encodeLoginLogoutOutMsg AskForExistingUser = "AskForExistingUser"
-- encodeLoginLogoutOutMsg RegisterError = "RegisterError"
-- encodeLoginLogoutOutMsg (RegisteredSuccessfully userId connId) = "RegisteredSuccessfully;" <> userId <> ";" <> connId
-- encodeLoginLogoutOutMsg LoginError = "LoginError"
-- encodeLoginLogoutOutMsg (LoginSuccessfully userId connId) = "LoginSuccessfully;" <> userId <> ";" <> connId
-- encodeLoginLogoutOutMsg (LogoutSuccessfully userId connId) = "LogoutSuccessfully;" <> userId <> ";" <> connId
-- encodeLoginLogoutOutMsg (NewAnonUser userId connId passwd) = "NewAnonUser;" <> userId <> ";" <> connId <> ";" <> passwd
-- encodeLoginLogoutOutMsg (OldAnonUser userId connId) = "OldAnonUser;" <> userId <> ";" <> connId

-- encodeRoomMsg :: RoomMsg -> String
-- encodeRoomMsg (GameRoomCreated roomId) = "GameRoomCreated;" <> roomId
-- encodeRoomMsg (GameRoomIsAlreadyActive roomId) = "GameRoomIsAlreadyActive;" <> roomId



decodeWebSocketOutputMessage :: String -> Maybe WebSocketOutputMessage
decodeWebSocketOutputMessage input =
  case (strs `index` 0) of
    Just "ResendIncorrect" -> Just ResendIncorrectOutMsg
    Just "GameRoom" -> GameRoomOutMsg <$> decodeRoomMsg (drop 1 strs)
    Just "LoginLogout" -> LoginLogoutOutMsg <$> decodeLoginLogoutOutMsg (drop 1 strs)
    _ -> Nothing
  where
    strs = split (Pattern ";")  input

decodeLoginLogoutOutMsg :: Array String -> Maybe LoginLogoutMsg
decodeLoginLogoutOutMsg input =
  case input of
    ["AskForExistingUser"] -> Just AskForExistingUser
    ["RegisterError"] -> Just RegisterError
    ["RegisteredSuccessfully", userId, connId] -> Just $ RegisteredSuccessfully userId connId
    ["LoginError"] -> Just LoginError
    ["LoginSuccessfully", userId, connId] -> Just $ LoginSuccessfully userId connId
    ["LogoutSuccessfully", userId, connId] -> Just $ LogoutSuccessfully userId connId
    ["NewAnonUser", userId, connId, passwd] -> Just $ NewAnonUser userId connId passwd
    ["OldAnonUser", userId, connId] -> Just $ OldAnonUser userId connId
    _ -> Nothing

decodeRoomMsg :: Array String -> Maybe RoomMsg
decodeRoomMsg input =
  case input of
    ["GameRoomCreated", roomId] -> Just $ GameRoomCreated roomId
    ["GameRoomIsAlreadyActive", roomId] -> Just $ GameRoomIsAlreadyActive roomId
    _ -> Nothing

