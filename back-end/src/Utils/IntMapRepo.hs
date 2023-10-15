{-# LANGUAGE NamedFieldPuns #-}

module Utils.IntMapRepo where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

data RepoIntMap v = RepoIntMap
  { repoMap :: IntMap v,
    maxId :: Int
  }

empty :: RepoIntMap v
empty =
  RepoIntMap
    { repoMap = IntMap.empty,
      maxId = 0
    }

add :: RepoIntMap v -> v -> (RepoIntMap v, Int)
add repo@RepoIntMap {repoMap, maxId} newElem =
  let newRepoMap = IntMap.insert maxId newElem repoMap
      newRepo = repo {repoMap = newRepoMap, maxId = maxId + 1}
   in (newRepo, maxId)

findById :: RepoIntMap v -> Int -> Maybe v
findById RepoIntMap {repoMap} idx = IntMap.lookup idx repoMap

update :: RepoIntMap v -> Int -> v -> RepoIntMap v
update repo@RepoIntMap {repoMap} idx newConnState =
  repo {repoMap = IntMap.insert idx newConnState repoMap}

delete :: RepoIntMap v -> Int -> RepoIntMap v
delete repo@RepoIntMap {repoMap} idx =
  repo {repoMap = IntMap.delete idx repoMap}