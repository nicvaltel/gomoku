module Utils.Utils where

import qualified Data.Text as T
import Text.Read (readMaybe)

data LgSeverity
  = LgApplication -- for inform at start. e.g. Listening at: 0.0.0.0:1234; GamesList at: 0.0.0.0:1235; Websocket server at 0.0.0.0:1235
  | LgCritical
  | LgError
  | LgInfo -- e.g. printf "Connected %s" (show connId)
  | LgMessage
  | LgConnection
  | LgDebug

logger :: LgSeverity -> String -> IO ()
logger _ = print

tshow :: Show a => a -> T.Text
tshow = T.pack . show

tReadMaybe :: Read a => T.Text -> Maybe a
tReadMaybe = readMaybe . T.unpack