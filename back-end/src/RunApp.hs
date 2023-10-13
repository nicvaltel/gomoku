{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RunApp where

import Configuration.Dotenv (parseFile)
import Data.Maybe (fromJust)
-- import qualified PostgreSQLConnector as PG
-- import qualified Server.WebSocketServerImpl as WSS
import Text.Printf (printf)
-- import Users.UserPostgresAdapter (UserRepoDB(UserRepoDB))
import Control.Exception (SomeException, catch)
import Domain.Types

runApp :: FilePath -> IO ()
runApp envFile = do
  env <- parseFile envFile
  let host :: Host = fromJust $ lookup "HOST" env
  let port :: Port = read . fromJust $ lookup "PORT" env
  let pingTime :: PingTime = read . fromJust $ lookup "PING_TIME" env
  putStrLn $ (printf "Listening at: %s:%d" host port :: String)
  pure ()
--   PG.initDBConn env $ \poolConn -> do
--     WSS.runWebSocketServer host port pingTime (UserRepoDB poolConn)

  

-- result :: [(Int, Text, Int, Text)] <- PG.withDBConn poolDBConn $ \conn -> query conn qryStr (Only (1 :: Int))
-- print result
