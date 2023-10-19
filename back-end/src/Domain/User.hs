{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.User where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)

type Username = Text

type Password = Text

data RegStatus = Registered | Anonim
  deriving (Show, Eq, Ord)

newtype UserId (r :: RegStatus) = UserId {unUserId :: Int}
  deriving (Show, Generic, Eq, Ord)

type AnyUserId = Either (UserId 'Anonim) (UserId 'Registered)

data User (r :: RegStatus) = User {userUserId :: UserId r, userName :: Username}
  deriving (Show, Eq, Ord)

-- | CRUD + checkPassword
class Monad m => UsersRepo m where
  addRegUser :: Username -> Password -> m (Maybe (UserId 'Registered))
  addRegUserToDB :: Username -> Password -> m (Maybe (UserId 'Registered))
  addAnonUser :: m (Maybe (UserId 'Anonim))
  findAnyUserById :: AnyUserId -> m (Maybe (Either (User 'Anonim) (User 'Registered)))
  updateRegUser :: UserId 'Registered -> User 'Registered -> m ()
  deleteRegUser :: UserId 'Registered -> m ()
  deleteAnonUser :: UserId 'Anonim -> m ()
  checkPassword :: UserId 'Registered -> Password -> m Bool

class GetRegStatus (r :: RegStatus) where
  getRegStatus :: UserId r -> RegStatus

instance GetRegStatus 'Registered where
  getRegStatus _ = Registered

instance GetRegStatus 'Anonim where
  getRegStatus _ = Anonim

instance FromJSON (UserId s)
instance ToJSON (UserId s)