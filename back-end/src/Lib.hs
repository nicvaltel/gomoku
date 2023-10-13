{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import qualified Adapter.InMemory.ConnectionsMem as C
import qualified Adapter.InMemory.UsersMem as M
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (runReaderT))
import Domain.Connection (ConnectionsRepo (..))
import Domain.User ( UsersRepo(..) )

type AppState = (M.UsersDB, C.ConnsDB)

newtype App a = App
  { unApp :: ReaderT AppState IO a
  }
  deriving (Applicative, Functor, Monad, MonadReader AppState, MonadIO)

instance UsersRepo App where
  addRegUser = M.addRegUser
  addAnonUser = M.addAnonUser
  findAnyUserById = M.findAnyUserById
  updateRegUser = M.updateRegUser
  deleteRegUser = M.deleteRegUser
  deleteAnonUser = M.deleteAnonUser
  checkPassword = M.checkPassword

instance ConnectionsRepo App where
  addConn = C.addConn
  findConnById = C.findConnById
  updateConn = C.updateConn
  deleteConn = C.deleteConn

runApp :: App a -> IO a
runApp app = do
  appStateUserDB <- M.emptyUserDB
  appStateConnDB <- C.emptyConnDB
  -- let allRepos = All
  runReaderT (unApp app) (appStateUserDB, appStateConnDB)