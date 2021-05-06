module Task (Task (..), pickTask, runTask) where

import Console (requestM, request_)
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
import NewProfile (getNewProfile)
import Profile (Profile)
import qualified Profile
import Text.Parsec (char, digit, eof, many, oneOf, parse, (<|>))
import Text.Parsec.Char (letter)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Task
    = NewProfile
    | Quit
    | StartProfile Profile

-- Parses a string into a task. If a task can be parsed, it is returned, otherwise Nothing is returned.
parseTask :: Map Int Profile -> String -> Maybe Task
parseTask _ "q" = Just Quit
parseTask _ "new" = Just NewProfile
parseTask profiles other = number >>= (`Map.lookup` profiles) <&> StartProfile
  where
    number = readMaybe other :: Maybe Int

-- Picks a task and returns it.
pickTask :: FP.FilePath -> IO Task
pickTask basePath = do
    profiles <- Profile.list basePath <&> zip [1 ..]
    let profileMap = fromList profiles
    profiles & forM_ $ \(idx, p) -> putStrLn $ printf "%d: %s" idx (Profile.name p)
    request_ (Just "Please choose a profile ('new' to create a new one, 'q' to quit)") Nothing (parseTask profileMap)

-- Runs a task.
runTask :: FP.FilePath -> Task -> IO ()
runTask basePath Quit = pure ()
runTask basePath NewProfile =
    requestM (Just "Please provide a name for the new profile") (getNewProfile basePath)
        >>= Profile.create
        >>= Profile.start
runTask basePath (StartProfile profile) = Profile.start profile