module Domain.WebSocketServer where
import Domain.User
import Domain.Types




class Monad m => WebSocketServer m where
    runWebSockerSerwer :: Host -> Port -> PingTime -> m()
    -- runWebSocketServer :: String -> Int -> PingTime -> UserRepoDB -> IO ()

--   getConnRepo :: wss -> crepo
--   getGameRoomRepo :: wss -> grrepo
--   getUserRepo :: wss -> urepo
--   webSocketServer :: wss -> PingTime -> WS.ServerApp
--   wsThreadMessageListener :: wss -> WS.Connection -> ConnectionId -> IO ()
--   checkForExistingUser :: wss -> WS.Connection -> IO UserId