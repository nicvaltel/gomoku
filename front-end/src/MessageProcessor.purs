module MessageProcessor
  ( messageProcessor
  )
  where

-- spago build
-- live-server .


import Prelude ((<>),($),(==),(/=), (||), (&&), otherwise)
import Data.Tuple(Tuple(..))
import Data.Maybe (Maybe(..))

import InputMessages
import OutputMessages
import Types


type AppData = 
  { connConf :: ConnectionConfig,
    updateCookieFlag :: Boolean
  }

type ConnectionConfig =
  { webSocketUrl :: String
  , userId :: String
  , connId :: String
  , userRegOrAnon :: String
  , tempAnonPasswd :: String
  }
  
-- updateConnectionConfig :: ConnectionConfig -> ConnectionConfig
-- updateConnectionConfig config =
--   config {userId = "newUserId", connId = "newConnId", tempAnonPasswd = "newPassword" }


handshakeSendData :: UserId -> RegOrAnon -> Password -> ConnId -> Handshake
handshakeSendData uId userRegOrAnon pwd cId 
  | uId == "" || pwd == "" = NonExisting
  | cId == "" && userRegOrAnon == "RegUser" = ExistingRegisteredUserNewConn uId pwd
  | cId /= "" && userRegOrAnon == "RegUser" = ExistingRegisteredUserAndConn cId uId pwd
  | cId /= "" && userRegOrAnon == "AnonUser" = ExistingAnonConn cId uId pwd
  | otherwise = NonExisting


processLoginLogoutMsg :: AppData -> LoginLogoutMsg -> Tuple AppData String
processLoginLogoutMsg appData AskForExistingUser = Tuple appData (encodeWebSocketInputMessage $ HandshakeInMsg $ handshakeSendData appData.connConf.userId appData.connConf.userRegOrAnon appData.connConf.tempAnonPasswd appData.connConf.connId)
  -- case appData.connConf.userId of
  --   "" -> Tuple appData (encodeWebSocketInputMessage $ HandshakeInMsg $ ExistingAnonConn "666" "666" "Hello, Password!")
  --   uId -> Tuple appData (encodeWebSocketInputMessage $ HandshakeInMsg $ ExistingAnonConn uId "999" "HELLO, DARLING!")
processLoginLogoutMsg appData RegisterError = Tuple appData ""
processLoginLogoutMsg appData (RegisteredSuccessfully userId connId) = Tuple appData ""
processLoginLogoutMsg appData LoginError = Tuple appData ""
processLoginLogoutMsg appData (LoginSuccessfully userId connId) = Tuple appData ""
processLoginLogoutMsg appData (LogoutSuccessfully userId connId) = Tuple appData ""
processLoginLogoutMsg appData (NewAnonUser userId connId passwd) =
  let newConnConf = appData.connConf {userId = userId, connId = connId, tempAnonPasswd = passwd, userRegOrAnon = "AnonUser"}
  in Tuple appData{ connConf = newConnConf, updateCookieFlag = true} ""
processLoginLogoutMsg appData (OldAnonUser userId connId) = Tuple appData "" -- that's correct

messageProcessor ::  AppData -> String -> Tuple AppData String
messageProcessor appData message = 
    case decodeWebSocketOutputMessage message of
        Nothing -> Tuple appData ""
        Just ResendIncorrectOutMsg -> Tuple appData ""
        Just (GameRoomOutMsg roomMsg) -> Tuple appData ""
        Just (LoginLogoutOutMsg loginLogoutMsg) -> processLoginLogoutMsg appData loginLogoutMsg


