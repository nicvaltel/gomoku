{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.PostgreSQL.RoomsDB
  ( archiveRoom,
    archiveAnonRoom,
    archiveRegisteredRoom,
  )
where

import Control.Monad.RWS (MonadIO (liftIO), MonadReader, asks)
import Data.Has (Has (getter))
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection, Only (Only), query)
import Domain.GameLogic (GameBoardState, GameResult, GameType, gameBoardStateToText, gameResultToText, indexOfGameTypeId)
import Domain.Room (Room (..))
import qualified Domain.Room as DR
import Domain.User (RegStatus (Registered), UserId (..))
import qualified PostgreSQLConnector as PG

type InPostgres reader m = (Has (Pool Connection) reader, MonadReader reader m, MonadIO m)

archiveRoom :: (DR.RoomsRepo m, InPostgres reader m) => DR.RoomId 'DR.ActiveRoom -> m (Maybe (DR.RoomId 'DR.FinishedRoom))
archiveRoom roomId = do
  mbActiveRoom <- DR.findActiveRoomById roomId
  case mbActiveRoom of
    Just Room {roomGameType, roomCreator, roomBoardState, roomGameResult} -> do
      case roomCreator of
        Right userId -> archiveRegisteredRoom userId roomGameType roomBoardState roomGameResult
        Left _ -> archiveAnonRoom roomGameType roomBoardState roomGameResult
    Nothing -> pure Nothing

archiveAnonRoom :: (DR.RoomsRepo m, InPostgres reader m) => GameType -> GameBoardState -> GameResult -> m (Maybe (DR.RoomId 'DR.FinishedRoom))
archiveAnonRoom gameType roomBoardState roomGameResult = do
  poolConn <- asks getter
  res :: [Only Int] <- liftIO $ PG.withDBConn poolConn $ \conn -> query conn queryStr (indexOfGameTypeId gameType, gameBoardStateToText roomBoardState, gameResultToText roomGameResult)
  case res of
    [Only archivedRoomId] -> pure $ Just (DR.RoomId archivedRoomId)
    _ -> pure Nothing
  where
    queryStr = "INSERT INTO gomoku_hub.rooms (game_type_id, anon_creator, board_state, game_result) VALUES(?,TRUE,?,?) returning id;"

archiveRegisteredRoom :: (DR.RoomsRepo m, InPostgres reader m) => UserId 'Registered -> GameType -> GameBoardState -> GameResult -> m (Maybe (DR.RoomId 'DR.FinishedRoom))
archiveRegisteredRoom (UserId uId) gameType roomBoardState roomGameResult = do
  poolConn <- asks getter
  res :: [Only Int] <- liftIO $ PG.withDBConn poolConn $ \conn -> query conn queryStr (indexOfGameTypeId gameType, uId, gameBoardStateToText roomBoardState, gameResultToText roomGameResult)
  case res of
    [Only archivedRoomId] -> pure $ Just (DR.RoomId archivedRoomId)
    _ -> pure Nothing
  where
    queryStr = "INSERT INTO gomoku_hub.rooms (game_type_id, creator_user_id, anon_creator, board_state, game_result) VALUES(?,?, FALSE,?,?) returning id;"
