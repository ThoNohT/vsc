module Main where

import Environment (ensureBasePathExists, getBasePath)
import Task (Task (..), pickTask, runTask)
import Text.Printf (printf)

main :: IO ()
main = do
    basePath <- getBasePath
    ensureBasePathExists basePath
    pickTask basePath >>= runTask basePath