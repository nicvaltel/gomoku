{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.InMemory.ConnectionsMem
  ( ConnsDB,
    addConn,
    findConnById,
    updateConn,
    deleteConn,
    emptyConnDB,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad.RWS (MonadIO (liftIO), MonadReader, asks)
import Data.Has (Has (getter))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Domain.Connection (connStateUserId, connStateWSConnection, connStatus)
import qualified Domain.Connection as DC
import Domain.User (AnyUserId)
import qualified Network.WebSockets as WS

type ConnsDB = TVar ConnectionsIntMap

type InMemory reader m = (Has (TVar ConnectionsIntMap) reader, MonadReader reader m, MonadIO m)

addConn :: InMemory reader m => WS.Connection -> AnyUserId -> DC.ConnStatus -> m DC.ConnId
addConn connStateWSConnection connStateUserId connStatus = do
  let connState = DC.ConnState {connStateWSConnection, connStateUserId, connStatus}
  tvar <- asks getter
  liftIO $ atomically $ do
    connsMap :: ConnectionsIntMap <- readTVar tvar
    let (newConnsMap, newConnId) = addConn' connsMap connState
    writeTVar tvar newConnsMap
    pure newConnId

findConnById :: InMemory reader m => DC.ConnId -> m (Maybe DC.ConnState)
findConnById connId = do
  tvar <- asks getter
  connsMap :: ConnectionsIntMap <- liftIO $ readTVarIO tvar
  pure $ findConnById' connsMap connId

updateConn :: InMemory reader m => DC.ConnId -> DC.ConnState -> m ()
updateConn connId newConnState = do
  tvar <- asks getter
  liftIO $ atomically $ do
    connsMap :: ConnectionsIntMap <- readTVar tvar
    let newConnsMap = updateConn' connsMap connId newConnState
    writeTVar tvar newConnsMap

deleteConn :: InMemory reader m => DC.ConnId -> m ()
deleteConn connId = do
  tvar <- asks getter
  liftIO $ atomically $ do
    connsMap :: ConnectionsIntMap <- readTVar tvar
    let newConnsMap = deleteConn' connsMap connId
    writeTVar tvar newConnsMap

emptyConnDB :: IO ConnsDB
emptyConnDB =
  newTVarIO
    ConnectionsIntMap
      { connectionsMap = IntMap.empty,
        maxConnectionId = DC.ConnId 0
      }

data ConnectionsIntMap = ConnectionsIntMap
  { connectionsMap :: IntMap DC.ConnState,
    maxConnectionId :: DC.ConnId
  }

addConn' :: ConnectionsIntMap -> DC.ConnState -> (ConnectionsIntMap, DC.ConnId)
addConn' connsMap@ConnectionsIntMap {connectionsMap, maxConnectionId} connState =
  let cId@(DC.ConnId maxConId_) = maxConnectionId
      newConsMap = IntMap.insert maxConId_ connState connectionsMap
      newConnectionsIntMap = connsMap {connectionsMap = newConsMap, maxConnectionId = DC.ConnId (maxConId_ + 1)}
   in (newConnectionsIntMap, cId)

findConnById' :: ConnectionsIntMap -> DC.ConnId -> Maybe DC.ConnState
findConnById' ConnectionsIntMap {connectionsMap} (DC.ConnId connId) = IntMap.lookup connId connectionsMap

updateConn' :: ConnectionsIntMap -> DC.ConnId -> DC.ConnState -> ConnectionsIntMap
updateConn' connsMap@ConnectionsIntMap {connectionsMap} (DC.ConnId connId) newConnState =
  connsMap {connectionsMap = IntMap.insert connId newConnState connectionsMap}

deleteConn' :: ConnectionsIntMap -> DC.ConnId -> ConnectionsIntMap
deleteConn' connsMap@ConnectionsIntMap {connectionsMap} (DC.ConnId connId) =
  connsMap {connectionsMap = IntMap.delete connId connectionsMap}