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
  deriving (Show, Eq, Ord)

data Room (s :: RoomStatus) = Room
  { roomGameType :: GameType,
    roomCreator :: AnyUserId,
    roomCreatorUsername :: Username,
    roomOpponents :: [AnyUserId],
    roomChat :: [(AnyUserId, Text, Timestamp)],
    roomGameActions :: [(AnyUserId, GameMove, Timestamp)],
    roomBoardState :: GameBoardState,
    roomGameResult :: GameResult
  }
  deriving (Show, Eq, Ord)

class Monad m => RoomsRepo m where
  createLobbyRoom :: AnyUserId -> Username -> GameType -> m (Room 'LobbyRoom, RoomId 'LobbyRoom)
  findLobbyRoomById :: RoomId 'LobbyRoom -> m (Maybe (Room 'LobbyRoom))
  updateLobbyRoom :: RoomId 'LobbyRoom -> Room 'LobbyRoom -> m ()
  deleteLobbyRoom :: RoomId 'LobbyRoom -> m ()
  runActiveRoom :: RoomId 'LobbyRoom -> m (Maybe (Room 'ActiveRoom, RoomId 'ActiveRoom))
  findActiveRoomById :: RoomId 'ActiveRoom -> m (Maybe (Room 'ActiveRoom))
  archiveRoom :: RoomId 'ActiveRoom -> m (Maybe (RoomId 'FinishedRoom))
  getLobbyRoomsList :: m [(Room 'LobbyRoom, RoomId 'LobbyRoom)]

mkNewRoom :: AnyUserId -> Username -> GameType -> Room 'LobbyRoom
mkNewRoom anyUserId username gameType =
  Room
    { roomGameType = gameType,
      roomCreator = anyUserId,
      roomCreatorUsername = username,
      roomOpponents = [],
      roomChat = [],
      roomGameActions = [],
      roomBoardState = newGameBoardState gameType,
      roomGameResult = GameResult
    }

lobbyRoomToActive :: Room 'LobbyRoom -> Room 'ActiveRoom
lobbyRoomToActive Room {roomGameType, roomCreator, roomCreatorUsername, roomOpponents, roomChat, roomGameActions, roomBoardState, roomGameResult} =
  Room {roomGameType, roomCreator, roomCreatorUsername, roomOpponents, roomChat, roomGameActions, roomBoardState, roomGameResult}

activeRoomToFinished :: Room 'ActiveRoom -> Room 'FinishedRoom
activeRoomToFinished Room {roomGameType, roomCreator, roomCreatorUsername, roomOpponents, roomChat, roomGameActions, roomBoardState, roomGameResult} =
  Room {roomGameType, roomCreator, roomCreatorUsername, roomOpponents, roomChat, roomGameActions, roomBoardState, roomGameResult}
