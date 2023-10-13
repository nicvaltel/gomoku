module Main (main) where

import qualified RunApp

main :: IO ()
main = do 
    RunApp.runApp "config.env"

