module HSPG.Architecture.Model.Users where
import Data.Map.Strict (Map)

data User = User {userId :: Int, userName :: String}

type UsersMap = Map Int User

class UsersRepo m where
    makeNewUser :: Int -> String -> m User
    getUser :: Int -> m (Maybe User)