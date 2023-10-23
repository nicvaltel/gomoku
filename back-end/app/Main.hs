module Main (main) where

-- websocat -v ws://127.0.0.1:1234

import qualified RunApp

main :: IO ()
main = do 
    RunApp.runApp "config.env"

