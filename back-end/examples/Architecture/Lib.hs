{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HSPG.Architecture.Lib where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import qualified HSPG.Architecture.Model.Games as DG
import qualified HSPG.Architecture.Model.Users as DU

import qualified HSPG.Architecture.Adapter.GamesMem as AG
import qualified HSPG.Architecture.Adapter.UsersMem as AU
import qualified HSPG.Architecture.Adapter.Repos as AR
import Control.Monad (ap)
import Control.Monad.Reader.Class (ask, MonadReader (local))
import Control.Monad.IO.Class (liftIO)



newtype App a = App {unApp :: ReaderT AR.Repos IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AR.Repos)

-- instance Functor App where
--   fmap f = App . fmap f . unApp

-- instance Applicative App where
--   pure = App . pure
--   (<*>) = ap

-- instance Monad App where
--   app >>= k = App $ do
--       a <- unApp app
--       unApp $ k a

-- instance MonadReader AR.Repos App where
--   ask = App ask
--   local f app = App $ local f (unApp app)


-- instance MonadIO App where
--   liftIO = App . liftIO



instance DU.UsersRepo App where
  makeNewUser = AU.makeNewUserIO
  getUser = AU.getUserIO

instance  DG.GamesRepo App where
  makeNewGame = AG.makeNewGameIO

