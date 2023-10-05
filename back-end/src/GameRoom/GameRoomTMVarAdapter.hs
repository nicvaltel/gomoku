{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}

module GameRoom.GameRoomTMVarAdapter
  ( GameRoomRepoTMVar (..),
  )
where

import Control.Concurrent.STM (TMVar, atomically, newTMVarIO, putTMVar, takeTMVar, readTMVar)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import GameLogic.GameLogic
  ( GameType,
    newGameBoardState,
  )
import GameRoom.GameRoom
import Users.User 

newtype GameRoomRepoTMVar = GameRoomRepoTMVar (TMVar NotStartedRoomsMap, TMVar InProgressRoomsMap)

instance GameRoomRepo GameRoomRepoTMVar where
  createGameRoomRepo :: IO GameRoomRepoTMVar
  createGameRoomRepo = do
    notStarted <- newTMVarIO (Map.empty :: Map RoomId (GameRoom 'GameNotStarted))
    inProgress <- newTMVarIO (Map.empty :: Map RoomId (GameRoom 'GameInProgress))
    pure $ GameRoomRepoTMVar (notStarted, inProgress)

  createGameRoom ::  GameRoomRepoTMVar -> AnyUserId -> GameType -> IO CreatedGameRoomId
  createGameRoom (GameRoomRepoTMVar (tmvRepo, _)) anyUserId gameType = do
    let newRoom = newGameRoom anyUserId gameType (newGameBoardState gameType)
    atomically $ do
      repo <- takeTMVar tmvRepo
      case Map.lookup anyUserId repo of
        Nothing -> do
          let newRepo = Map.insert anyUserId newRoom repo
          putTMVar tmvRepo newRepo
          pure (NewCreatedRoom anyUserId)
        Just _ -> do 
          putTMVar tmvRepo repo
          pure (AlreadyActiveRoom anyUserId) -- gameroom for current user is already active

  findActiveGameRoom :: GameRoomRepoTMVar -> RoomId -> IO (Maybe (GameRoom 'GameInProgress))
  findActiveGameRoom (GameRoomRepoTMVar (_, tmvRepo)) roomId = do
     repo <- atomically $ readTMVar tmvRepo
     case Map.lookup roomId repo of
        Nothing -> pure Nothing
        Just room -> pure (Just room) 