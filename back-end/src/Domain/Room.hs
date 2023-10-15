{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

module Domain.Room where

import Data.Text (Text)
import Domain.GameLogic
import Domain.Types (Timestamp)
import Domain.User

data RoomStatus = LobbyRoom | ActiveRoom | FinishedRoom
  deriving (Show, Eq, Ord)

newtype RoomId (s :: RoomStatus) = RoomId {unRoomId :: Int}

data Room (s :: RoomStatus) = Room
  { roomGameType :: GameType,
    roomUsers :: [AnyUserId],
    roomChat :: [(AnyUserId, Text)],
    roomGameActions :: [(AnyUserId, GameMove, Timestamp)],
    roomBoardState :: GameBoardState,
    gameResult :: Maybe GameResult
  }
  deriving (Show, Eq, Ord)

class Monad m => GameRoomRepo m where
  createLobbyRoom :: AnyUserId -> GameType -> m (Room 'LobbyRoom, RoomId 'LobbyRoom)
  findLobbyRoomById :: RoomId 'LobbyRoom -> m (Maybe (Room 'LobbyRoom))
  updateLobbyRoom :: Room 'LobbyRoom -> m ()
  deleteLobbyRoom :: RoomId 'LobbyRoom -> m Bool
  runActiveRoom :: RoomId 'LobbyRoom -> m (Maybe (Room 'ActiveRoom, RoomId 'ActiveRoom))
  findActiveRoomById :: RoomId 'ActiveRoom -> m (Maybe (Room 'ActiveRoom))
  archiveRoom :: RoomId 'ActiveRoom -> m (Maybe (Room 'FinishedRoom, RoomId 'FinishedRoom))

-- findActiveRooms :: m [(RoomId s, GameType)]

mkNewRoom :: AnyUserId -> GameType -> Room 'LobbyRoom
mkNewRoom anyUserId gameType =
  Room
    { roomGameType = gameType,
      roomUsers = [anyUserId],
      roomChat = [],
      roomGameActions = [],
      roomBoardState = newGameBoardState gameType,
      gameResult = Nothing
    }

lobbyRoomToActive :: Room 'LobbyRoom -> Room 'ActiveRoom
lobbyRoomToActive Room {roomGameType, roomUsers, roomChat, roomGameActions, roomBoardState, gameResult} =
  Room {roomGameType, roomUsers, roomChat, roomGameActions, roomBoardState, gameResult}

activeRoomToFinished :: Room 'ActiveRoom -> Room 'FinishedRoom
activeRoomToFinished Room {roomGameType, roomUsers, roomChat, roomGameActions, roomBoardState, gameResult} =
  Room {roomGameType, roomUsers, roomChat, roomGameActions, roomBoardState, gameResult}