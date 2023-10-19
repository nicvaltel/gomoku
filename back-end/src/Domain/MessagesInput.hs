{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.MessagesInput where

import Domain.User (Username, Password)
import Data.Text (Text)
import Data.Aeson(FromJSON, ToJSON, encode, decode)
import GHC.Generics (Generic)

data WebSocketInputMessage
  = LogInOutInMsg LogInOut
  | InitJoinRoomInMsg InitJoinRoom
  | GameActionInMsg GameAction
  | IncorrectInMsg Text
  | AnswerExistingUserInMsg AnswerExistingUser
  deriving (Show, Generic)

data LogInOut = Login Username Password | Logout | Register Username Password
  deriving (Show, Generic)

data InitJoinRoom = InitGameRoom Text | JoinGameRoom Text
  deriving (Show, Generic)

newtype GameAction = GameAction Text
  deriving (Show, Generic)

data AnswerExistingUser = ExistingAnon Int | ExistingRegisteredUser Int Text | NonExistingUser
  deriving (Show, Generic)

instance FromJSON LogInOut
instance FromJSON InitJoinRoom
instance FromJSON GameAction
instance FromJSON AnswerExistingUser
instance FromJSON WebSocketInputMessage

instance ToJSON LogInOut
instance ToJSON InitJoinRoom
instance ToJSON GameAction
instance ToJSON AnswerExistingUser
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

test :: IO ()
test = do
  let username = "USER_111"
  let password = "PWD_111"
  let loginOut = encode $ LogInOutInMsg $ Register username password
  print loginOut
  let xloginOut :: Maybe WebSocketInputMessage = decode loginOut
  print xloginOut
  pure ()