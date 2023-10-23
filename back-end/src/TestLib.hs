{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module TestLib where

import qualified LibRepos
import Domain.User 
import Domain.Room ( RoomsRepo(..), RoomId )
import Control.Monad.IO.Class (liftIO)
import Domain.GameLogic (GameType(..))
import Data.Maybe (fromJust)


testUser :: LibRepos.App (UserId 'Registered, Username)
testUser = do
    mbUserId <- addRegUser "Vasyan2" "Bubu112233"
    let userId = fromJust mbUserId
    liftIO $ print userId
    ch1 <- checkPassword userId "Bubu112233"
    ch2 <- checkPassword userId "Bubu"
    liftIO $ print ch1
    liftIO $ print ch2
    pure (userId,"Vasyan2")


testRoom :: UserId 'Registered -> Username -> LibRepos.App ()
testRoom userId username = do
    (roomLobby, roomLobbyId) <- createLobbyRoom (Right userId) username (GameType "10+5" "Casual")
    mbActive <- runActiveRoom roomLobbyId
    let (roomActive, roomActiveId) = fromJust mbActive
    mbFinished <- archiveRoom roomActiveId
    let roomFinishedId = fromJust mbActive
    liftIO $ print roomFinishedId
    pure ()


testAll :: LibRepos.App ()
testAll = do
    (userId, username) <- testUser
    testRoom userId username
