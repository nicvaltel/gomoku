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
    checkPassword,
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
import Utils.Utils (tshow)

type UsersDB = TVar UsersIntMap

type InMemory reader m = (Has (TVar UsersIntMap) reader, MonadReader reader m, MonadIO m)

addRegUser :: InMemory reader m => Username -> Password -> m (Maybe (UserId 'Registered)) -- TODO save hashed password
addRegUser username password = do
  tvar <- asks getter
  liftIO $ atomically $ do
    usersMap :: UsersIntMap <- readTVar tvar
    let (newUsersMap, newUserId) = addRegUser' username usersMap
    writeTVar tvar newUsersMap
    pure (Just newUserId)

addAnonUser :: InMemory reader m => m (Maybe (UserId 'Anonim))
addAnonUser = do
  tvar <- asks getter
  liftIO $ atomically $ do
    usersMap :: UsersIntMap <- readTVar tvar
    let (newUsersMap, newUserId) = addAnonUser' "Anon" usersMap
    writeTVar tvar newUsersMap
    pure (Just newUserId)

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

checkPassword :: InMemory reader m => UserId r -> Password -> m Bool
checkPassword uId passwd = undefined -- TODO implement

emptyUserDB :: IO UsersDB
emptyUserDB =
  newTVarIO
    UsersIntMap
      { regUsers = IntMap.empty,
        anonUsers = IntMap.empty,
        maxRegId = UserId 0,
        maxAnonId = UserId 0
      }

data UsersIntMap = UsersIntMap
  { regUsers :: IntMap (User 'Registered),
    anonUsers :: IntMap (User 'Anonim),
    maxRegId :: UserId 'Registered,
    maxAnonId :: UserId 'Anonim
  }

addRegUser' :: Username -> UsersIntMap -> (UsersIntMap, UserId 'Registered)
addRegUser' username inputMap =
  let regUsers_ = regUsers inputMap
      uId@(UserId maxRegId_) = maxRegId inputMap
      newUser = User {userId = uId, userName = username}
      newRegUsers = IntMap.insert maxRegId_ newUser regUsers_
      newIntMap = inputMap {regUsers = newRegUsers, maxRegId = UserId (maxRegId_ + 1)}
   in (newIntMap, uId)

addAnonUser' :: Username -> UsersIntMap -> (UsersIntMap, UserId 'Anonim)
addAnonUser' username inputMap =
  let anonUsers_ = anonUsers inputMap
      uId@(UserId maxAnonId_) = maxAnonId inputMap
      newUserName = username <> tshow maxAnonId_
      newUser = User {userId = uId, userName = newUserName}
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