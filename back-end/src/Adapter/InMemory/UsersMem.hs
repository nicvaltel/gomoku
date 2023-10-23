{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.InMemory.UsersMem
  ( UsersDB,
    addRegUser,
    addAnonUser,
    findAnyUserById,
    updateRegUser,
    deleteRegUser,
    deleteAnonUser,
    emptyUserDB,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, asks)
import Data.Has (Has (getter))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Domain.User
  ( AnyUserId,
    Password,
    RegStatus (Anonim, Registered),
    User (..),
    UserId (..),
    Username,
  )
import qualified Domain.User as DU
import Utils.Utils (tshow)

type UsersDB = TVar UsersIntMap

type InMemory reader m = (Has UsersDB reader, MonadReader reader m, MonadIO m)

data UsersIntMap = UsersIntMap
  { regUsers :: IntMap (User 'Registered),
    anonUsers :: IntMap (User 'Anonim),
    maxAnonId :: UserId 'Anonim
  }

emptyUserDB :: IO UsersDB
emptyUserDB =
  newTVarIO
    UsersIntMap
      { regUsers = IntMap.empty,
        anonUsers = IntMap.empty,
        maxAnonId = UserId 0
      }

addRegUser :: (DU.UsersRepo m, InMemory reader m) => Username -> Password -> m (Maybe (UserId 'Registered))
addRegUser username password = do
  mbUserId <- DU.addRegUserToDB username password
  case mbUserId of
    Nothing -> pure Nothing
    Just userId -> do
      tvar <- asks getter
      liftIO $ atomically $ do
        usersMap :: UsersIntMap <- readTVar tvar
        let newUsersMap = addRegUser' userId username usersMap
        writeTVar tvar newUsersMap
        pure (Just userId)

addAnonUser :: InMemory reader m => m (UserId 'Anonim)
addAnonUser = do
  tvar <- asks getter
  liftIO $ atomically $ do
    usersMap :: UsersIntMap <- readTVar tvar
    let (newUsersMap, newUserId) = addAnonUser' "Anon" usersMap
    writeTVar tvar newUsersMap
    pure newUserId

findAnyUserById :: InMemory reader m => AnyUserId -> m (Maybe (Either (User 'Anonim) (User 'Registered)))
findAnyUserById anyUid = do
  tvar <- asks getter
  usersMap :: UsersIntMap <- liftIO $ readTVarIO tvar
  pure $ findAnyUser' anyUid usersMap

updateRegUser :: InMemory reader m => UserId 'Registered -> User 'Registered -> m ()
updateRegUser uId newUser = do
  tvar <- asks getter
  liftIO $ atomically $ do
    usersMap :: UsersIntMap <- readTVar tvar
    let newUsersMap = updateRegUser' uId newUser usersMap
    writeTVar tvar newUsersMap

deleteRegUser :: InMemory reader m => UserId 'Registered -> m ()
deleteRegUser uId = do
  tvar <- asks getter
  liftIO $ atomically $ do
    usersMap :: UsersIntMap <- readTVar tvar
    let newUsersMap = deleteRegUser' uId usersMap
    writeTVar tvar newUsersMap

deleteAnonUser :: InMemory reader m => UserId 'Anonim -> m ()
deleteAnonUser uId = do
  tvar <- asks getter
  liftIO $ atomically $ do
    usersMap :: UsersIntMap <- readTVar tvar
    let newUsersMap = deleteAnonUser' uId usersMap
    writeTVar tvar newUsersMap

addRegUser' :: UserId 'Registered -> Username -> UsersIntMap -> UsersIntMap
addRegUser' userUserId@(UserId uId) userName inputMap@UsersIntMap {regUsers} =
  let newUser = User {userUserId, userName}
      newRegUsers = IntMap.insert uId newUser regUsers
   in inputMap {regUsers = newRegUsers}

addAnonUser' :: Username -> UsersIntMap -> (UsersIntMap, UserId 'Anonim)
addAnonUser' username inputMap =
  let anonUsers_ = anonUsers inputMap
      uId@(UserId maxAnonId_) = maxAnonId inputMap
      newUserName = username <> tshow maxAnonId_
      newUser = User {userUserId = uId, userName = newUserName}
      newAnonUsers = IntMap.insert maxAnonId_ newUser anonUsers_
      newIntMap = inputMap {anonUsers = newAnonUsers, maxAnonId = UserId (maxAnonId_ + 1)}
   in (newIntMap, uId)

findAnyUser' :: AnyUserId -> UsersIntMap -> Maybe (Either (User 'Anonim) (User 'Registered))
findAnyUser' (Left userId) usersMap = Left <$> findAnonUser' userId usersMap
findAnyUser' (Right userId) usersMap = Right <$> findRegUser' userId usersMap

findRegUser' :: UserId 'Registered -> UsersIntMap -> Maybe (User 'Registered)
findRegUser' (UserId uId) UsersIntMap {regUsers} = IntMap.lookup uId regUsers

findAnonUser' :: UserId 'Anonim -> UsersIntMap -> Maybe (User 'Anonim)
findAnonUser' (UserId uId) UsersIntMap {anonUsers} = IntMap.lookup uId anonUsers

updateRegUser' :: UserId 'Registered -> User 'Registered -> UsersIntMap -> UsersIntMap
updateRegUser' (UserId uId) newUser umap@UsersIntMap {regUsers} = umap {regUsers = IntMap.insert uId newUser regUsers}

updateAnonUser' :: UserId 'Anonim -> User 'Anonim -> UsersIntMap -> UsersIntMap
updateAnonUser' (UserId uId) newUser umap@UsersIntMap {anonUsers} = umap {anonUsers = IntMap.insert uId newUser anonUsers}

deleteRegUser' :: UserId 'Registered -> UsersIntMap -> UsersIntMap
deleteRegUser' (UserId uId) umap@UsersIntMap {regUsers} = umap {regUsers = IntMap.delete uId regUsers}

deleteAnonUser' :: UserId 'Anonim -> UsersIntMap -> UsersIntMap
deleteAnonUser' (UserId uId) umap@UsersIntMap {anonUsers} = umap {anonUsers = IntMap.delete uId anonUsers}