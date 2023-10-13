{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Domain.Connection where

import Domain.User (AnyUserId)
import qualified Network.WebSockets as WS

newtype ConnId = ConnId {unConnId :: Int}
  deriving (Show)

data ConnStatus = NormalConnection | ConnectionNotFound
  deriving (Show)

data ConnState = ConnState {connStateWSConnection :: WS.Connection, connStateUserId :: AnyUserId, connStatus :: ConnStatus}

class Monad m => ConnectionsRepo m where
  addConn :: WS.Connection -> AnyUserId -> ConnStatus -> m ConnId
  findConnById :: ConnId -> m (Maybe ConnState)
  updateConn :: ConnId -> ConnState -> m ()
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
