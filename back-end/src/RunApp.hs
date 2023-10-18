{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RunApp where

import Configuration.Dotenv (parseFile)
import Data.Maybe (fromJust)
import qualified PostgreSQLConnector as PG
-- import qualified Server.WebSocketServerImpl as WSS
import Text.Printf (printf)
-- import Users.UserPostgresAdapter (UserRepoDB(UserRepoDB))
import Domain.Types
import qualified Lib

import qualified TestLib

runApp :: FilePath -> IO ()
runApp envFile = do
  env <- parseFile envFile
  
  let host :: Host = fromJust $ lookup "HOST" env
  let port :: Port = read . fromJust $ lookup "PORT" env
  let pingTime :: PingTime = read . fromJust $ lookup "PING_TIME" env
  
  PG.initDBConn env $ \poolConn -> do
    -- Main logic starts here
    putStrLn (printf "Listening at: %s:%d" host port :: String)
    Lib.runApp poolConn (TestLib.testAll)
    -- WSS.runWebSocketServer host port pingTime (UserRepoDB poolConn)
  pure ()
