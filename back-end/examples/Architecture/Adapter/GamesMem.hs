{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module HSPG.Architecture.Adapter.GamesMem where

import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Monad.Reader (MonadIO (liftIO), asks, MonadReader)
import qualified Data.Map.Strict as Map
import qualified HSPG.Architecture.Model.Games as DG
import qualified HSPG.Architecture.Model.Users as DU
import qualified HSPG.Architecture.Adapter.Repos as AR



makeNewGameIO :: (MonadIO m, DU.UsersRepo m, MonadReader AR.Repos m) => Int -> String -> Int-> m (Maybe DG.Game)
makeNewGameIO gid gamename userId = do
  mayUser <- DU.getUser userId
  case mayUser of
    Nothing -> pure Nothing
    Just user -> do
      let newGame = DG.Game gid gamename user
      tvar <- asks AR.repoGames
      liftIO $ atomically $ do
        gamesMap :: DG.GamesMap <- readTVar tvar
        let newGamesMap = Map.insert gid newGame gamesMap
        writeTVar tvar newGamesMap
      pure (Just newGame)
