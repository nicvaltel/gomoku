module Domain.GameLogic where

data GameMove = GameMove
  deriving (Show, Eq, Ord)

data StoneColor = Black | White
  deriving (Show, Eq, Ord)

data GameBoardState = GameBoardState
  { gbsCurrentMoveColor :: StoneColor, 
    gbsCurrentMoveNumber :: Int,
    gbsBoardMatrix :: [[Maybe StoneColor]],
    gbsLastStone :: Maybe (Int, Int)
  }
  deriving (Show, Eq, Ord)

data GameResult = GameResult
  deriving (Show, Eq, Ord)

data GameType = GameType
  deriving (Show, Eq, Ord)

newGameBoardState :: GameType -> GameBoardState
newGameBoardState _ =
  GameBoardState
    { gbsCurrentMoveColor = White,
      gbsCurrentMoveNumber = 0,
      gbsBoardMatrix = [[]],
      gbsLastStone = Nothing
    }