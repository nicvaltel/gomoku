module HSPG.Architecture.Model.Games where

import qualified HSPG.Architecture.Model.Users as DU
import Data.Map.Strict (Map)

data Game = Game {gameId :: Int, gameName :: String, gameUser :: DU.User}

type GamesMap = Map Int Game

class GamesRepo m where
    makeNewGame :: 
        Int -> -- game id
        String -> -- game name
        Int -> -- user id
        m (Maybe Game)