module InputMessages  where

import Prelude ((<>))
import Data.Array (index, drop)
import Data.String (split, Pattern(..))
import Data.Maybe (Maybe(..), maybe)
import Types



data WebSocketInputMessage
  = LogInOutInMsg LogInOut
  | InitJoinRoomInMsg InitJoinRoom
  | GameActionInMsg GameAction
  | IncorrectInMsg String
  | HandshakeInMsg Handshake

data LogInOut = Login Username Password | Logout | Register Username Password

data InitJoinRoom = InitGameRoom String | JoinGameRoom String

newtype GameAction = GameAction String

data Handshake
  = ExistingAnonConn ConnId UserId Password
  | ExistingRegisteredUserAndConn ConnId UserId Password
  | ExistingRegisteredUserNewConn UserId Password
  | NonExisting


-- decodeGameAction :: Array String -> Maybe GameAction
-- decodeGameAction strs = case strs of
--   ["GameAction", bstr] -> Just (GameAction bstr)
--   _ -> Nothing


-- decodeInitJoinRoom :: Array String -> Maybe InitJoinRoom
-- decodeInitJoinRoom msgs =
--   case msgs of
--     ["InitGameRoom", bstr] -> Just (InitGameRoom bstr)
--     ["JoinGameRoom", bstr] -> Just (JoinGameRoom bstr)
--     _ -> Nothing

-- decodeLogInOut :: Array String -> Maybe LogInOut
-- decodeLogInOut strs =
--   case strs of
--     ["Login", username, password] -> Just (Login username password)
--     ["Logout"] -> Just Logout
--     ["Register", username, password] -> Just (Register username password)
--     _ -> Nothing

-- decodeHandshake :: Array String -> Maybe Handshake
-- decodeHandshake strs = case strs of
--   ["ExistingAnonConn", connId, userId, password] -> Just (ExistingAnonConn connId userId password)
--   ["ExistingRegisteredUserAndConn", connId, userId, password] -> Just (ExistingRegisteredUserAndConn connId userId password)
--   ["ExistingRegisteredUserNewConn", userId, password] -> Just (ExistingRegisteredUserNewConn userId password)
--   ["NonExisting"] -> Just NonExisting
--   _ -> Nothing


-- decodeWebSocketInputMessage :: String -> WebSocketInputMessage
-- decodeWebSocketInputMessage "" = IncorrectInMsg "Empty list"
-- decodeWebSocketInputMessage str = case (strs `index` 0) of
--      Just "LogInOut" -> maybeToWSIM LogInOutInMsg (decodeLogInOut (drop 1 strs))
--      Just "InitJoinRoom" -> maybeToWSIM InitJoinRoomInMsg (decodeInitJoinRoom (drop 1 strs))
--      Just "GameAction" -> maybeToWSIM GameActionInMsg (decodeGameAction (drop 1 strs))
--      Just "Handshake" -> maybeToWSIM HandshakeInMsg (decodeHandshake (drop 1 strs))
--      _ -> IncorrectInMsg "Unknown message type"
--   where
--     strs = split (Pattern ";")  str

--     maybeToWSIM :: forall a. (a -> WebSocketInputMessage) -> Maybe a -> WebSocketInputMessage
--     maybeToWSIM constructor mbVal = maybe (IncorrectInMsg str) constructor mbVal
 



encodeGameAction :: GameAction -> String
encodeGameAction (GameAction bstr) = "GameAction;" <> bstr

encodeInitJoinRoom :: InitJoinRoom -> String
encodeInitJoinRoom (InitGameRoom bstr) = "InitGameRoom;" <> bstr
encodeInitJoinRoom (JoinGameRoom bstr) = "JoinGameRoom;" <> bstr

encodeLogInOut :: LogInOut -> String
encodeLogInOut (Login username password) = "Login;" <> username <> ";" <> password
encodeLogInOut Logout = "Logout"
encodeLogInOut (Register username password) = "Register;" <> username <> ";" <> password

encodeHandshake :: Handshake -> String
encodeHandshake (ExistingAnonConn connId userId password) = "ExistingAnonConn;" <> connId <> ";" <> userId <> ";" <> password
encodeHandshake (ExistingRegisteredUserAndConn connId userId password) = "ExistingRegisteredUserAndConn;" <> connId <> ";" <> userId <> ";" <> password
encodeHandshake (ExistingRegisteredUserNewConn userId password) = "ExistingRegisteredUserNewConn;" <> userId <> ";" <> password
encodeHandshake NonExisting = "NonExisting"

encodeWebSocketInputMessage :: WebSocketInputMessage -> String
encodeWebSocketInputMessage (LogInOutInMsg logInOut) = "LogInOut;" <> encodeLogInOut logInOut
encodeWebSocketInputMessage (InitJoinRoomInMsg initJoinRoom) = "InitJoinRoom;" <> encodeInitJoinRoom initJoinRoom
encodeWebSocketInputMessage (GameActionInMsg gameAction) = "GameAction;" <> encodeGameAction gameAction
encodeWebSocketInputMessage (HandshakeInMsg handshake) = "Handshake;" <> encodeHandshake handshake
encodeWebSocketInputMessage (IncorrectInMsg msg) = "Incorrect;" <> msg