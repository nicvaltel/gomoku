{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Adapter.WSGamesList
  ( wssGamesList,
    getLobbyGamesList,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Cont (MonadIO (liftIO))
import Data.Aeson (encode)
import Domain.Connection (ConnectionsRepo)
import qualified Domain.GameLogic as DGL
import Domain.Room (Room (roomCreatorUsername, roomGameType), RoomsRepo (..))
import Domain.Types
import Domain.User (UsersRepo)
import qualified Domain.User as DU
import qualified Network.WebSockets as WS

type WSSApp m = (MonadIO m, UsersRepo m, ConnectionsRepo m, RoomsRepo m)

wssGamesList :: WSSApp m => PingTime -> (WS.PendingConnection -> m ())
wssGamesList pingTime pending = do
  conn <- liftIO $ WS.acceptRequest pending
  lobbySenderIO <- lobbySender conn
  liftIO $
    WS.withPingThread conn pingTime (return ()) $ pure lobbySenderIO

lobbySender :: WSSApp m => WS.Connection -> m ()
lobbySender conn = forever $ do
  lobbyList <- getLobbyGamesList
  let lobbyMsg = map (\(username, gtype) -> (username, DGL.gameTypeTime gtype, DGL.gameTypeMode gtype)) lobbyList
  let message = encode lobbyMsg
  liftIO $ WS.sendTextData conn message
  liftIO $ threadDelay 500_000

getLobbyGamesList :: WSSApp m => m [(DU.Username, DGL.GameType)]
getLobbyGamesList =
  map (\(room, _) -> (roomCreatorUsername room, roomGameType room)) <$> getLobbyRoomsList