{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Connection where

import Domain.User (AnyUserId)
import qualified Network.WebSockets as WS
import Text.Printf (printf)
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import GHC.Generics (Generic)

newtype ConnId = ConnId {unConnId :: Int}
  deriving (Show, Generic)

data ConnStatus = NormalConnection | ConnectionNotFound
  deriving (Show)

data ConnState = ConnState {connStateWSConnection :: WS.Connection, connStateUserId :: AnyUserId, connStatus :: ConnStatus}

instance Show ConnState where
  show ConnState {connStateUserId, connStatus} =
    case connStateUserId of
      Left uId -> printf "ConnectionState{WS.Connection, connStateUserId = Anon %s , connStatus = %s}" (show uId) (show connStatus)
      Right uId -> printf "ConnectionState{WS.Connection, connStateUserId = Registered %s , connStatus = %s}" (show uId) (show connStatus)

class Monad m => ConnectionsRepo m where
  addConn :: WS.Connection -> AnyUserId -> ConnStatus -> m (ConnId, ConnState)
  findConnById :: ConnId -> m (Maybe ConnState)
  updateConn :: ConnId -> ConnState -> m (ConnId, ConnState)
  deleteConn :: ConnId -> m ()

  updateUserInConn :: ConnId -> AnyUserId -> m Bool
  updateUserInConn connId anyUserId = do
    mbConnSt <- findConnById connId
    case mbConnSt of
      Just connSt -> updateConn connId connSt {connStateUserId = anyUserId} >> pure True
      Nothing -> pure False

--   userIdFromConnectionId :: db -> ConnectionId -> IO (Maybe UserId)
--   getConnStatus :: db -> ConnectionId -> IO ConnectionStatus
--   nextAnonUserId :: db -> IO UserId


instance FromJSON ConnId

instance ToJSON ConnId