module GameLogic.GameLogic where


data GameMove

data StoneColor = Black | White
    deriving (Show, Eq)

data GameBoardState = GameBoardState {
    gbsCurrentMoveColor :: StoneColor,
    gbsCurrentMoveNumber :: Int,
    gbsBoardMatrix :: [[Maybe StoneColor]],
    gbsLastStone :: Maybe (Int,Int)
}

data GameResult

data GameType = GameType

newGameBoardState :: GameType -> GameBoardState
newGameBoardState _ = GameBoardState {
    gbsCurrentMoveColor = White,
    gbsCurrentMoveNumber = 0,
    gbsBoardMatrix = [[]],
    gbsLastStone = Nothing
}

