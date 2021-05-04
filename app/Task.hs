module Task (Task (..), pickTask) where

import Control.Arrow ((>>>))
import Control.Monad (forM_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find)
import Data.Text as T (Text, isPrefixOf)
import Environment (baseName)
import Filesystem (listDirectory)
import qualified Filesystem.Path.CurrentOS as FP (FilePath)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Task
    = NewProfile
    | Quit
    | StartProfile Text

-- Parses a string into a task. If a task can be parsed, it is returned, otherwise Nothing is returned.
parseTask :: [(Int, FP.FilePath)] -> String -> Maybe Task
parseTask subDirs "q" = Just Quit
parseTask subDirs "new" = Just NewProfile
parseTask subDirs other = number >>= (\n -> find (fst >>> (== n)) subDirs) <&> (snd >>> baseName >>> StartProfile)
  where
    number = readMaybe other :: Maybe Int

-- Picks a task and returns it.
pickTask :: FP.FilePath -> IO Task
pickTask basePath = do
    subDirs <- listDirectory basePath <&> filter (not . T.isPrefixOf "." . baseName) <&> zip [1 ..]
    subDirs & forM_ $ \(idx, sd) -> putStrLn $ printf "%d: %s" idx (baseName sd)
    readTaskFromInput subDirs

-- Reads a task from the console, if a task could not be parsed from the input, the task is asked again.
readTaskFromInput :: [(Int, FP.FilePath)] -> IO Task
readTaskFromInput subDirs = do
    putStrLn "Please choose a profile ('new' to create a new one, 'q' to quit)"
    task <- parseTask subDirs <$> getLine
    maybe (readTaskFromInput subDirs) pure task