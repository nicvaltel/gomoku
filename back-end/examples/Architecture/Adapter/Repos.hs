module HSPG.Architecture.Adapter.Repos where


import Control.Concurrent.STM (TVar)
import qualified HSPG.Architecture.Model.Games as DG
import qualified HSPG.Architecture.Model.Users as DU
import Control.Monad.Reader (ReaderT)

data Repos = Repos
  { repoUsers :: TVar DU.UsersMap,
    repoGames :: TVar DG.GamesMap
  }

type ReposApp = ReaderT Repos IO