module Main where

import Environment (ensureBasePathExists, getBasePath)
import Task (Task (..), pickTask)
import Text.Printf (printf)

main :: IO ()
main = do
    basePath <- getBasePath

    ensureBasePathExists basePath
    task <- pickTask basePath

    case task of
        Quit -> pure ()
        NewProfile -> putStrLn "Creating a new profile."
        StartProfile name -> putStrLn $ printf "Starting profile %s." name