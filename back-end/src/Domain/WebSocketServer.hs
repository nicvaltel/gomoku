module Domain.WebSocketServer where

import Data.Text (Text)
import Domain.Connection
import Domain.MessagesInput
import Domain.Types
import qualified Network.WebSockets as WS

class Monad m => WebSocketServer m where
  webSocketServer :: PingTime -> WS.PendingConnection -> m ()

  -- handshake :: WS.Connection -> m (ConnState, ConnId)
  processInputLogInOut :: ConnId -> ConnState -> LogInOut -> m ()
  processInputInitJoinRoom :: ConnId -> ConnState -> InitJoinRoom -> m ()
  processInputGameAction :: ConnId -> ConnState -> GameAction -> m ()
  processInputIncorrect :: ConnId -> ConnState -> Text -> m ()
  processInputAnswerExistingUser :: ConnId -> ConnState -> AnswerExistingUser -> m ()

-- runwebSocketServerApp :: Host -> Port -> PingTime -> Pool Connection -> IO ()
-- runwebSocketServerApp host port pingTime poolConn = do
--   appStateUserDB <- UM.emptyUserDB
--   appStateConnDB <- CM.emptyConnDB
--   appStateRoomDB <- RM.emptyRoomDB
--   let ioApp pending = runReaderT (unApp $ webSocketServer pingTime pending) (appStateUserDB, appStateConnDB,appStateRoomDB, poolConn)
--   WS.runServer host port ioApp