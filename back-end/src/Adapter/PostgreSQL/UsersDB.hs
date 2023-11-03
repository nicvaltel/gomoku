{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.PostgreSQL.UsersDB (addRegUserToDB, checkPassword) where

import Control.Monad.RWS (MonadIO (liftIO), MonadReader, asks)
import Data.Has (Has (getter))
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection, Only (Only), query)
import Domain.User hiding (addRegUserToDB, checkPassword)
import qualified PostgreSQLConnector as PG
import Domain.Types (Password)

type InPostgres reader m = (Has (Pool Connection) reader, MonadReader reader m, MonadIO m)

addRegUserToDB :: (UsersRepo m, InPostgres reader m) => Username -> Password -> m (Maybe (UserId 'Registered))
addRegUserToDB username passwd = do
  poolConn <- asks getter
  res :: [Only Int] <- liftIO $ PG.withDBConn poolConn $ \conn -> query conn queryStr (username, passwd)
  case res of
    [Only uId] -> pure $ Just (UserId uId)
    _ -> pure Nothing
  where
    queryStr = "INSERT INTO gomoku_hub.users (username, passwd, created) VALUES(?, crypt(?, gen_salt('bf')), (now() AT TIME ZONE 'utc'::text)) returning id;"

checkPassword :: (UsersRepo m, InPostgres reader m) => UserId 'Registered -> Password -> m Bool
checkPassword (UserId uId) passwd = do
  poolConn <- asks getter
  res :: [Only Int] <- liftIO $ PG.withDBConn poolConn $ \conn -> query conn queryStr (uId, passwd)
  case res of
    [Only uId] -> pure True
    _ -> pure False
  where
    queryStr = "SELECT id FROM gomoku_hub.users WHERE id = ? and passwd = crypt(?, passwd)"