module Domain.WebSocketServer where

import Domain.MessagesInput
import Domain.Connection
import Data.Text (Text)
import qualified Network.WebSockets as WS




class Monad m => WebSocketServer m where
    handshake :: WS.Connection -> m (ConnState, ConnId)
    processInputLogInOut :: ConnId -> ConnState -> LogInOut -> m ()
    processInputInitJoinRoom :: ConnId -> ConnState -> InitJoinRoom -> m ()
    processInputGameAction :: ConnId -> ConnState -> GameAction -> m ()
    processInputIncorrect :: ConnId -> ConnState -> Text -> m ()
    processInputAnswerExistingUser :: ConnId -> ConnState -> AnswerExistingUser -> m ()
  

    -- runWebSockerSerwer :: Host -> Port -> PingTime -> m()
    -- runWebSocketServer :: String -> Int -> PingTime -> UserRepoDB -> IO ()

--   getConnRepo :: wss -> crepo
--   getGameRoomRepo :: wss -> grrepo
--   getUserRepo :: wss -> urepo
--   webSocketServer :: wss -> PingTime -> WS.ServerApp
--   wsThreadMessageListener :: wss -> WS.Connection -> ConnectionId -> IO ()
--   checkForExistingUser :: wss -> WS.Connection -> IO UserId

