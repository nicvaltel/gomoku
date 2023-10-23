{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

module Domain.Room where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Domain.GameLogic
import Domain.Types (Timestamp)
import Domain.User
import GHC.Generics (Generic)

data RoomStatus = LobbyRoom | ActiveRoom | FinishedRoom
  deriving (Show, Eq, Ord)

newtype RoomId (s :: RoomStatus) = RoomId {unRoomId :: Int}
  deriving (Show, Generic, Eq, Ord)

data Room (s :: RoomStatus) = Room
  { roomGameType :: GameType,
    roomCreator :: AnyUserId,
    roomOpponents :: [AnyUserId],
    roomChat :: [(AnyUserId, Text, Timestamp)],
    roomGameActions :: [(AnyUserId, GameMove, Timestamp)],
    roomBoardState :: GameBoardState,
    roomGameResult :: GameResult
  }
  deriving (Show, Eq, Ord)

class Monad m => RoomsRepo m where
  createLobbyRoom :: AnyUserId -> GameType -> m (Room 'LobbyRoom, RoomId 'LobbyRoom)
  findLobbyRoomById :: RoomId 'LobbyRoom -> m (Maybe (Room 'LobbyRoom))
  updateLobbyRoom :: RoomId 'LobbyRoom -> Room 'LobbyRoom -> m ()
  deleteLobbyRoom :: RoomId 'LobbyRoom -> m ()
  runActiveRoom :: RoomId 'LobbyRoom -> m (Maybe (Room 'ActiveRoom, RoomId 'ActiveRoom))
  findActiveRoomById :: RoomId 'ActiveRoom -> m (Maybe (Room 'ActiveRoom))
  archiveRoom :: RoomId 'ActiveRoom -> m (Maybe (RoomId 'FinishedRoom))

mkNewRoom :: AnyUserId -> GameType -> Room 'LobbyRoom
mkNewRoom anyUserId gameType =
  Room
    { roomGameType = gameType,
      roomCreator = anyUserId,
      roomOpponents = [],
      roomChat = [],
      roomGameActions = [],
      roomBoardState = newGameBoardState gameType,
      roomGameResult = GameResult
    }

lobbyRoomToActive :: Room 'LobbyRoom -> Room 'ActiveRoom
lobbyRoomToActive Room {roomGameType, roomCreator, roomOpponents, roomChat, roomGameActions, roomBoardState, roomGameResult} =
  Room {roomGameType, roomCreator, roomOpponents, roomChat, roomGameActions, roomBoardState, roomGameResult}

activeRoomToFinished :: Room 'ActiveRoom -> Room 'FinishedRoom
activeRoomToFinished Room {roomGameType, roomCreator, roomOpponents, roomChat, roomGameActions, roomBoardState, roomGameResult} =
  Room {roomGameType, roomCreator, roomOpponents, roomChat, roomGameActions, roomBoardState, roomGameResult}

instance FromJSON (RoomId s)

instance ToJSON (RoomId s)
