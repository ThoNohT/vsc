module Task (Task (..), pickTask, runTask) where

import Control.Arrow ((>>>))
import Control.Monad (forM_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict as Map (Map, fromList, lookup)
import Data.Text as T (Text, isPrefixOf)
import Environment (baseName)
import Filesystem (listDirectory)
import qualified Filesystem.Path.CurrentOS as FP (FilePath)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.Map (toList)
import GHC.Exts (sortWith)

data Task
    = NewProfile
    | Quit
    | StartProfile Text

-- Parses a string into a task. If a task can be parsed, it is returned, otherwise Nothing is returned.
parseTask :: Map Int FP.FilePath  -> String -> Maybe Task
parseTask subDirs "q" = Just Quit
parseTask subDirs "new" = Just NewProfile
parseTask subDirs other = number >>= (`Map.lookup` subDirs) <&> (baseName >>> StartProfile)
  where
    number = readMaybe other :: Maybe Int

-- Picks a task and returns it.
pickTask :: FP.FilePath -> IO Task
pickTask basePath = do
    subDirs <- listDirectory basePath <&> filter (not . T.isPrefixOf "." . baseName) <&> zip [1 ..] <&> fromList
    subDirs & toList & sortWith fst & forM_ $ \(idx, sd) -> putStrLn $ printf "%d: %s" idx (baseName sd)
    readTaskFromInput subDirs

-- Reads a task from the console, if a task could not be parsed from the input, the task is asked again.
readTaskFromInput :: Map Int FP.FilePath  -> IO Task
readTaskFromInput subDirs = do
    putStrLn "Please choose a profile ('new' to create a new one, 'q' to quit)"
    task <- parseTask subDirs <$> getLine
    maybe (readTaskFromInput subDirs) pure task

-- Runs a task.
runTask :: Task -> IO ()
runTask Quit = pure ()

runTask NewProfile = do
    pure ()

runTask (StartProfile profile) =
    pure ()