{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Domain.MessagesInput where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Text (Text)
import Domain.User (Username, UserId(..), RegStatus (..))
import GHC.Generics (Generic)
import qualified Network.WebSockets as WS
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (decodeUtf8)
import Utils.Utils
import Domain.Connection (ConnId(..))
import Domain.Types (Password)

data WebSocketInputMessage
  = LogInOutInMsg LogInOut
  | InitJoinRoomInMsg InitJoinRoom
  | GameActionInMsg GameAction
  | IncorrectInMsg Text
  | HandshakeInMsg Handshake
  deriving (Show, Generic)

data LogInOut = Login Username Password | Logout | Register Username Password
  deriving (Show, Generic)

data InitJoinRoom = InitGameRoom Text | JoinGameRoom Text
  deriving (Show, Generic)

newtype GameAction = GameAction Text
  deriving (Show, Generic)

data Handshake = 
  ExistingAnonConn ConnId (UserId 'Anonim)
  | ExistingRegisteredUserAndConn ConnId (UserId 'Registered) Password 
  | ExistingRegisteredUserNewConn (UserId 'Registered) Password 
  | NonExisting
  deriving (Show, Generic)

instance FromJSON LogInOut

instance FromJSON InitJoinRoom

instance FromJSON GameAction

instance FromJSON Handshake

instance FromJSON WebSocketInputMessage

instance ToJSON LogInOut

instance ToJSON InitJoinRoom

instance ToJSON GameAction

instance ToJSON Handshake

instance ToJSON WebSocketInputMessage

-- instance ToJSON Person

-- instance FromJSON LogInOut where
--   parseJSON (Object v) = do
--     tag <- v .: "tag"
--     case tag :: Text of
--       "Login" -> Login <$> v .: "loginUserName" <*> v .: "loginPassword"
--       "Logout" -> pure Logout
--       "Register" -> Register <$> v .: "registerUsername" <*> v .: "registerPassword"
--       _ -> fail "Invalid tag"
--   parseJSON _ = fail "Invalid JSON format"


receiveWebSocketInMsg :: String -> WS.Connection -> IO WebSocketInputMessage
receiveWebSocketInMsg logPrefix conn = do
  input <-  WS.receiveData conn
  logger LgMessage $ logPrefix ++ show input
  let txt :: Text = decodeUtf8 input
  case decode (fromStrict input) of
    Nothing -> pure (IncorrectInMsg txt)
    Just inputMsg -> pure inputMsg


test :: IO ()
test = do
  let username = "USER_111"
  let password = "PWD_111"
  let loginOut = encode $ LogInOutInMsg $ Register username password
  print loginOut
  let xloginOut :: Maybe WebSocketInputMessage = decode loginOut
  print xloginOut
  pure ()


-- liftIO $ print $ encode $ HandshakeInMsg (ExistingAnonConn (ConnId 777) (UserId 888))