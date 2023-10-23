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
import Domain.Connection (connStateUserId, connStateWSConnection, connStatus)
import qualified Domain.Connection as DC
import Domain.User (AnyUserId)
import qualified Network.WebSockets as WS
import Utils.IntMapRepo (RepoIntMap)
import qualified Utils.IntMapRepo as IntMapRepo

type ConnectionsIntMap = RepoIntMap DC.ConnState

type ConnsDB = TVar ConnectionsIntMap

type InMemory reader m = (Has ConnsDB reader, MonadReader reader m, MonadIO m)

emptyConnDB :: IO ConnsDB
emptyConnDB = newTVarIO IntMapRepo.empty

addConn :: InMemory reader m => WS.Connection -> AnyUserId -> DC.ConnStatus -> m (DC.ConnId, DC.ConnState)
addConn connStateWSConnection connStateUserId connStatus = do
  let connState = DC.ConnState {connStateWSConnection, connStateUserId, connStatus}
  tvar <- asks getter
  liftIO $ atomically $ do
    connsMap :: ConnectionsIntMap <- readTVar tvar
    let (newConnsMap, newConnId) = IntMapRepo.add connsMap connState
    writeTVar tvar newConnsMap
    pure (DC.ConnId newConnId, connState)

findConnById :: InMemory reader m => DC.ConnId -> m (Maybe DC.ConnState)
findConnById (DC.ConnId connId) = do
  tvar <- asks getter
  connsMap :: ConnectionsIntMap <- liftIO $ readTVarIO tvar
  pure $ IntMapRepo.findById connsMap connId

updateConn :: InMemory reader m => DC.ConnId -> DC.ConnState -> m ()
updateConn (DC.ConnId connId) newConnState = do
  tvar <- asks getter
  liftIO $ atomically $ do
    connsMap :: ConnectionsIntMap <- readTVar tvar
    let newConnsMap = IntMapRepo.update connsMap connId newConnState
    writeTVar tvar newConnsMap

deleteConn :: InMemory reader m => DC.ConnId -> m ()
deleteConn (DC.ConnId connId) = do
  tvar <- asks getter
  liftIO $ atomically $ do
    connsMap :: ConnectionsIntMap <- readTVar tvar
    let newConnsMap = IntMapRepo.delete connsMap connId
    writeTVar tvar newConnsMap