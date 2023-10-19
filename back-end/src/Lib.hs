{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import qualified Adapter.InMemory.ConnectionsMem as CM
import qualified Adapter.InMemory.UsersMem as UM
import qualified Adapter.InMemory.RoomsMem as RM
import qualified Adapter.PostgreSQL.RoomsDB as RPG
import qualified Adapter.PostgreSQL.UsersDB as UPG
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT (runReaderT))
import Domain.Connection (ConnectionsRepo (..))
import Domain.User ( UsersRepo(..) )
import Domain.Room (RoomsRepo(..))
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Domain.Types
import qualified Network.WebSockets as WS

type AppState = (UM.UsersDB, CM.ConnsDB, RM.RoomsDB, Pool Connection)

newtype App a = App
  { unApp :: ReaderT AppState IO a
  }
  deriving (Applicative, Functor, Monad, MonadReader AppState, MonadIO)

instance UsersRepo App where
  addRegUser = UM.addRegUser
  addAnonUser = UM.addAnonUser
  findAnyUserById = UM.findAnyUserById
  updateRegUser = UM.updateRegUser
  deleteRegUser = UM.deleteRegUser
  deleteAnonUser = UM.deleteAnonUser
  checkPassword = UPG.checkPassword
  addRegUserToDB = UPG.addRegUserToDB

instance ConnectionsRepo App where
  addConn = CM.addConn
  findConnById = CM.findConnById
  updateConn = CM.updateConn
  deleteConn = CM.deleteConn

instance RoomsRepo App where
  createLobbyRoom = RM.createLobbyRoom
  findLobbyRoomById = RM.findLobbyRoomById
  updateLobbyRoom = RM.updateLobbyRoom
  deleteLobbyRoom = RM.deleteLobbyRoom
  runActiveRoom = RM.runActiveRoom
  findActiveRoomById = RM.findActiveRoomById
  archiveRoom = RPG.archiveRoom


-- runWebSocketServer :: Host -> Port -> PingTime -> App ()
-- runWebSocketServer host port pingTime = do
--         liftIO $ WS.runServer host port $ \pending -> runReaderT (unApp (webSocketServer pingTime pending))
--         pure ()






runApp :: Pool Connection -> App a -> IO a
runApp poolConn app = do
  appStateUserDB <- UM.emptyUserDB
  appStateConnDB <- CM.emptyConnDB
  appStateRoomDB <- RM.emptyRoomDB
  runReaderT (unApp app) (appStateUserDB, appStateConnDB,appStateRoomDB, poolConn)



webSocketServer :: PingTime -> WS.PendingConnection -> App ()
webSocketServer pingTime pending = do
  conn <- liftIO $ WS.acceptRequest pending 
  pure ()

runwebSocketServerApp :: Host -> Port -> PingTime -> Pool Connection -> IO ()
runwebSocketServerApp host port pingTime poolConn = do
  appStateUserDB <- UM.emptyUserDB
  appStateConnDB <- CM.emptyConnDB
  appStateRoomDB <- RM.emptyRoomDB
  let ioApp pending = runReaderT (unApp $ webSocketServer pingTime pending) (appStateUserDB, appStateConnDB,appStateRoomDB, poolConn)
  WS.runServer host port ioApp

-- runwebSocketServerAnyApp :: Host -> Port -> PingTime -> Pool Connection -> (WS.PendingConnection -> App ()) -> IO ()
-- runwebSocketServerAnyApp host port pingTime poolConn app = do
--   appStateUserDB <- UM.emptyUserDB
--   appStateConnDB <- CM.emptyConnDB
--   appStateRoomDB <- RM.emptyRoomDB
--   let ioApp pending = runReaderT (unApp $ app pending) (appStateUserDB, appStateConnDB,appStateRoomDB, poolConn)
--   WS.runServer host port ioApp