{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Domain.Room where

import Data.Text (Text)
import Domain.GameLogic
import Domain.Types (Timestamp)
import Domain.User


type RoomId r = UserId r

data RoomStatus = WaitingForOponent | GameInProgress | GameFinished
  deriving (Show, Eq, Ord)

data Room r (s :: RoomStatus) = Room
  { gameRoomId :: RoomId r,
    roomGameType :: GameType,
    roomUsers :: [UserId r],
    roomChat :: [(UserId r, Text)],
    roomGameActions :: [(UserId r, GameMove, Timestamp)],
    roomRoomActions :: [(UserId r, GameMove, Timestamp)],
    roomBoardState :: GameBoardState,
    gameResult :: Maybe GameResult
  }
  deriving (Show, Eq, Ord)

class Monad m => GameRoomRepo m where
  createRoom :: UserId r -> GameType -> m (Room r 'WaitingForOponent)
  findRoomById :: UserId r -> m (Maybe (Room r s))
  updateRoom :: Room r s -> m Bool
  deleteRoom :: RoomId r -> m Bool
  findActiveRooms :: m [(RoomId r, GameType)]
