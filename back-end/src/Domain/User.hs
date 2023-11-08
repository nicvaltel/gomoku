{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Domain.User where

import Data.Text (Text)
import Domain.Types (Password)

type Username = Text

data RegStatus = Registered | Anonim
  deriving (Show, Eq, Ord)

newtype UserId (r :: RegStatus) = UserId {unUserId :: Int}
  deriving (Show, Eq, Ord)

type AnyUserId = Either (UserId 'Anonim) (UserId 'Registered)

data User (r :: RegStatus) = User {userUserId :: UserId r, userName :: Username, password :: Password}
  deriving (Show, Eq, Ord)

-- | CRUD + checkPassword
class Monad m => UsersRepo m where
  addRegUser :: Username -> Password -> m (Maybe (UserId 'Registered))
  addRegUserToDB :: Username -> Password -> m (Maybe (UserId 'Registered))
  addAnonUser :: m (UserId 'Anonim, Password)
  findAnyUserById :: AnyUserId -> m (Maybe (Either (User 'Anonim) (User 'Registered)))
  updateRegUser :: UserId 'Registered -> User 'Registered -> m ()
  deleteRegUser :: UserId 'Registered -> m ()
  deleteAnonUser :: UserId 'Anonim -> m ()
  checkPassword :: UserId 'Registered -> Password -> m Bool
  checkAnonPassword :: UserId 'Anonim -> Password -> m Bool

class GetRegStatus (r :: RegStatus) where
  getRegStatus :: UserId r -> RegStatus

instance GetRegStatus 'Registered where
  getRegStatus _ = Registered

instance GetRegStatus 'Anonim where
  getRegStatus _ = Anonim