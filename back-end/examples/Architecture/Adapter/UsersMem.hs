{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module HSPG.Architecture.Adapter.UsersMem where

import Control.Concurrent.STM (atomically, readTVar, writeTVar, readTVarIO)
import Control.Monad.Reader (MonadIO (liftIO), asks, MonadReader)
import qualified Data.Map.Strict as Map
import qualified HSPG.Architecture.Model.Users as DU
import qualified HSPG.Architecture.Adapter.Repos as AR

makeNewUserIO :: (MonadIO m, MonadReader AR.Repos m) => Int -> String -> m DU.User
makeNewUserIO uid username = do
  let newUser = DU.User uid username
  tvar <- asks AR.repoUsers
  liftIO $ atomically $ do
    usersMap :: DU.UsersMap <- readTVar tvar
    let newUsersMap = Map.insert uid newUser usersMap
    writeTVar tvar newUsersMap
  pure newUser

getUserIO :: (MonadIO m, MonadReader AR.Repos m) => Int -> m (Maybe DU.User)
getUserIO uId = do
  tvar <- asks AR.repoUsers
  usersMap :: DU.UsersMap <- liftIO $ readTVarIO tvar
  pure (Map.lookup uId usersMap)
