{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.InMemory.RoomsMem
  ( RoomsDB,
    emptyRoomDB,
    createLobbyRoom,
    findLobbyRoomById,
    updateLobbyRoom,
    deleteLobbyRoom,
    runActiveRoom,
    findActiveRoomById,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad.RWS (MonadIO (liftIO), MonadReader, asks)
import Data.Has (Has (getter))
import Domain.GameLogic
import qualified Domain.Room as DR
import Domain.User
import Utils.IntMapRepo (RepoIntMap)
import qualified Utils.IntMapRepo as IntMapRepo

type RoomsIntMap (s :: DR.RoomStatus) = RepoIntMap (DR.Room s)

data RoomsDB = RoomsDB
  { roomDBLobby :: TVar (RoomsIntMap 'DR.LobbyRoom),
    roomDBActive :: TVar (RoomsIntMap 'DR.ActiveRoom),
    roomDBFinished :: TVar (RoomsIntMap 'DR.FinishedRoom)
  }

type InMemory reader m = (Has RoomsDB reader, MonadReader reader m, MonadIO m)

emptyRoomDB :: IO RoomsDB
emptyRoomDB = do
  roomDBLobby <- newTVarIO IntMapRepo.empty
  roomDBActive <- newTVarIO IntMapRepo.empty
  roomDBFinished <- newTVarIO IntMapRepo.empty
  pure RoomsDB {roomDBLobby, roomDBActive, roomDBFinished}

createLobbyRoom :: InMemory reader m => AnyUserId -> GameType -> m (DR.Room 'DR.LobbyRoom, DR.RoomId 'DR.LobbyRoom)
createLobbyRoom anyUserId gameType = do
  let newRoom = DR.mkNewRoom anyUserId gameType
  tvar <- asks (roomDBLobby . getter)
  liftIO $ atomically $ do
    lobbyRooms :: RoomsIntMap 'DR.LobbyRoom <- readTVar tvar
    let (newLobbyRooms, newRoomId) = IntMapRepo.add lobbyRooms newRoom
    writeTVar tvar newLobbyRooms
    pure (newRoom, DR.RoomId newRoomId)

findLobbyRoomById :: InMemory reader m => DR.RoomId 'DR.LobbyRoom -> m (Maybe (DR.Room 'DR.LobbyRoom))
findLobbyRoomById (DR.RoomId rId) = do
  tvar <- asks (roomDBLobby . getter)
  lobbyRooms :: RoomsIntMap 'DR.LobbyRoom <- liftIO $ readTVarIO tvar
  pure $ IntMapRepo.findById lobbyRooms rId

updateLobbyRoom :: InMemory reader m => DR.RoomId 'DR.LobbyRoom -> DR.Room 'DR.LobbyRoom -> m ()
updateLobbyRoom (DR.RoomId rId) newRoom = do
  tvar <- asks (roomDBLobby . getter)
  liftIO $ atomically $ do
    lobbyRooms :: RoomsIntMap 'DR.LobbyRoom <- readTVar tvar
    let newLobbyRooms = IntMapRepo.update lobbyRooms rId newRoom
    writeTVar tvar newLobbyRooms

deleteLobbyRoom :: InMemory reader m => DR.RoomId 'DR.LobbyRoom -> m ()
deleteLobbyRoom (DR.RoomId rId) = do
  tvar <- asks (roomDBLobby . getter)
  liftIO $ atomically $ do
    lobbyRooms :: RoomsIntMap 'DR.LobbyRoom <- readTVar tvar
    let newLobbyRooms = IntMapRepo.delete lobbyRooms rId
    writeTVar tvar newLobbyRooms

runActiveRoom :: InMemory reader m => DR.RoomId 'DR.LobbyRoom -> m (Maybe (DR.Room 'DR.ActiveRoom, DR.RoomId 'DR.ActiveRoom))
runActiveRoom roomId = do
  mbLobbyRoom <- findLobbyRoomById roomId
  case mbLobbyRoom of
    Just lobbyRoom -> do
      let newActiveRoom = DR.lobbyRoomToActive lobbyRoom
      tvarActive <- asks (roomDBActive . getter)
      liftIO $ atomically $ do
        activeRooms :: RoomsIntMap 'DR.ActiveRoom <- readTVar tvarActive
        let (newActiveRooms, newActiveRoomId) = IntMapRepo.add activeRooms newActiveRoom
        writeTVar tvarActive newActiveRooms
        pure $ Just (newActiveRoom, DR.RoomId newActiveRoomId)
    Nothing -> pure Nothing

findActiveRoomById :: InMemory reader m => DR.RoomId 'DR.ActiveRoom -> m (Maybe (DR.Room 'DR.ActiveRoom))
findActiveRoomById (DR.RoomId rId) = do
  tvar <- asks (roomDBActive . getter)
  activeRooms :: RoomsIntMap 'DR.ActiveRoom <- liftIO $ readTVarIO tvar
  pure $ IntMapRepo.findById activeRooms rId