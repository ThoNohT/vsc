module Task (Task (..), pickTask, runTask) where

import Console (request_)
import Control.Arrow ((>>>))
import Control.Monad (forM_)
import Data.Either (isRight)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (toList)
import Data.Map.Strict as Map (Map, fromList, lookup)
import Data.Text as T (Text, isPrefixOf)
import Environment (baseName)
import Filesystem (listDirectory)
import qualified Filesystem.Path.CurrentOS as FP (FilePath)
import GHC.Exts (sortWith)
import Task.NewProfile (runNewProfile)
import Text.Parsec (char, digit, eof, many, oneOf, parse, (<|>))
import Text.Parsec.Char (letter)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Task
    = NewProfile
    | Quit
    | StartProfile Text

-- Parses a string into a task. If a task can be parsed, it is returned, otherwise Nothing is returned.
parseTask :: Map Int FP.FilePath -> String -> Maybe Task
parseTask subDirs "q" = Just Quit
parseTask subDirs "new" = Just NewProfile
parseTask subDirs other = number >>= (`Map.lookup` subDirs) <&> (baseName >>> StartProfile)
  where
    number = readMaybe other :: Maybe Int

-- Picks a task and returns it.
pickTask :: FP.FilePath -> IO (Map Int FP.FilePath, Task)
pickTask basePath = do
    subDirs <- listDirectory basePath <&> filter (not . T.isPrefixOf "." . baseName) <&> zip [1 ..] <&> fromList
    subDirs & toList & sortWith fst & forM_ $ \(idx, sd) -> putStrLn $ printf "%d: %s" idx (baseName sd)
    task <- request_ (Just "Please choose a profile ('new' to create a new one, 'q' to quit)") Nothing (parseTask subDirs)
    pure (subDirs, task)

-- Runs a task.
runTask :: (Map Int FP.FilePath, Task) -> IO ()
runTask (_, Quit) = pure ()
runTask (subDirs, NewProfile) = runNewProfile
runTask (subDirs, StartProfile profile) = pure ()