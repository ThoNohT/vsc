module Task (Task (..), readTaskFromInput) where

import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Data.List (find)
import Data.Text (Text)
import FileSystem (baseName)
import Filesystem.Path.CurrentOS as FP (FilePath)
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

-- Reads a task from the console, if a task could not be parsed from the input, the task is asked again.
readTaskFromInput :: [(Int, FP.FilePath)] -> IO Task
readTaskFromInput subDirs = do
    putStrLn "Please choose a profile ('new' to create a new one, 'q' to quit)"
    task <- parseTask subDirs <$> getLine
    maybe (readTaskFromInput subDirs) pure task