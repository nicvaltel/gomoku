{-# LANGUAGE OverloadedStrings #-}

module Domain.GameLogic where

import Data.Text.Lazy (Text)

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

data GameResult = Unfinished | GameResult
  deriving (Show, Eq, Ord)

type GameTime = String

type GameMode = String

data GameType = GameType {gameTypeTime :: GameTime, gameTypeMode :: GameMode}
  deriving (Show, Eq, Ord)

newGameBoardState :: GameType -> GameBoardState
newGameBoardState _ =
  GameBoardState
    { gbsCurrentMoveColor = White,
      gbsCurrentMoveNumber = 0,
      gbsBoardMatrix = [[]],
      gbsLastStone = Nothing
    }

indexOfGameTypeId :: GameType -> Int
indexOfGameTypeId (GameType _ _) = 1

gameBoardStateToText :: GameBoardState -> Text
gameBoardStateToText _ = "GameBoardState" -- TODO implement

gameResultToText :: GameResult -> Text
gameResultToText Unfinished = "Unfinished" -- TODO implement
gameResultToText GameResult = "GameResult"