{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.MessagesInput where

import qualified Data.ByteString.Char8 as BSC8
import Data.Text.Encoding (decodeUtf8)
import Domain.Connection (ConnId (..))
import Domain.Types (MessageStr, Password)
import Domain.User (RegStatus (..), UserId (..), Username)
import qualified Network.WebSockets as WS
import Text.Read (readMaybe)
import Utils.Utils

data WebSocketInputMessage
  = LogInOutInMsg LogInOut
  | InitJoinRoomInMsg InitJoinRoom
  | GameActionInMsg GameAction
  | IncorrectInMsg MessageStr
  | HandshakeInMsg Handshake
  deriving (Show)

data LogInOut = Login Username Password | Logout | Register Username Password
  deriving (Show)

data InitJoinRoom = InitGameRoom MessageStr | JoinGameRoom MessageStr
  deriving (Show)

newtype GameAction = GameAction MessageStr
  deriving (Show)

data Handshake
  = ExistingAnonConn ConnId (UserId 'Anonim) Password
  | ExistingRegisteredUserAndConn ConnId (UserId 'Registered) Password
  | ExistingRegisteredUserNewConn (UserId 'Registered) Password
  | NonExisting
  deriving (Show)

toWebSocketInputMessage :: MessageStr -> WebSocketInputMessage
toWebSocketInputMessage bstr =
  let bstrs = BSC8.split ';' bstr
   in case bstrs of
        [] -> IncorrectInMsg bstr
        ("LogInOut" : rest) -> maybeToWSIM LogInOutInMsg (toLogInOut rest)
        ("InitJoinRoom" : rest) -> maybeToWSIM InitJoinRoomInMsg (toInitJoinRoom rest)
        ("GameAction" : rest) -> maybeToWSIM GameActionInMsg (toGameAction rest)
        ("Handshake" : rest) -> maybeToWSIM HandshakeInMsg (toHandshake rest)
        _ -> IncorrectInMsg bstr
  where
    maybeToWSIM :: (a -> WebSocketInputMessage) -> Maybe a -> WebSocketInputMessage
    maybeToWSIM constructor mbVal = maybe (IncorrectInMsg bstr) constructor mbVal

toHandshake :: [MessageStr] -> Maybe Handshake
toHandshake = \case
  ["ExistingAnonConn", connId, userId, password] -> do
    cId :: Int <- readMaybe $ BSC8.unpack connId
    uId :: Int <- readMaybe $ BSC8.unpack userId
    pure (ExistingAnonConn (ConnId cId) (UserId uId) (decodeUtf8 password))
  ["ExistingRegisteredUserAndConn", connId, userId, password] -> do
    cId :: Int <- readMaybe $ BSC8.unpack connId
    uId :: Int <- readMaybe $ BSC8.unpack userId
    pure (ExistingRegisteredUserAndConn (ConnId cId) (UserId uId) (decodeUtf8 password))
  ["ExistingRegisteredUserNewConn", userId, password] -> do
    uId :: Int <- readMaybe $ BSC8.unpack userId
    pure (ExistingRegisteredUserNewConn (UserId uId) (decodeUtf8 password))
  ["NonExisting"] -> Just NonExisting
  _ -> Nothing

toGameAction :: [MessageStr] -> Maybe GameAction
toGameAction = \case
  ["GameAction", bstr] -> Just (GameAction bstr)
  _ -> Nothing

toInitJoinRoom :: [MessageStr] -> Maybe InitJoinRoom
toInitJoinRoom = \case
  ["InitGameRoom", bstr] -> Just (InitGameRoom bstr)
  ["JoinGameRoom", bstr] -> Just (JoinGameRoom bstr)
  _ -> Nothing

toLogInOut :: [MessageStr] -> Maybe LogInOut
toLogInOut = \case
  ["Login", username, password] -> Just (Login (decodeUtf8 username) (decodeUtf8 password))
  ["Logout"] -> Just Logout
  ["Register", username, password] -> Just (Register (decodeUtf8 username) (decodeUtf8 password))
  _ -> Nothing

receiveWebSocketInMsg :: String -> WS.Connection -> IO WebSocketInputMessage
receiveWebSocketInMsg logPrefix conn = do
  input <- WS.receiveData conn
  logger LgMessage $ logPrefix ++ show input
  print $ toWebSocketInputMessage input -- TODO remove it
  pure $ toWebSocketInputMessage input

test :: IO ()
test = do
  let loginOut = "LogInOut;Register;USER_111;PWD_111"
  let xloginOut = toWebSocketInputMessage loginOut
  print xloginOut
  pure ()
