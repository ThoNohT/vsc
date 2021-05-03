module Main where

import Control.Arrow ( (>>>) )
import Data.Functor ( (<&>) )
import Data.Function ( (&) )
import Data.Text (Text)
import qualified Data.Text as T
import Filesystem ( createTree, getHomeDirectory, isDirectory, listDirectory )
import Filesystem.Path.CurrentOS as FP ( append, encodeString, fromText, FilePath )
import Control.Monad (forM_)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.List (find)


data Task
    = NewProfile
    | Quit
    | StartProfile Text

main :: IO ()
main = do
    basePath <- getBasePath
    ensureBasePathExists basePath
    task <- pickTask basePath

    case task of
        Quit -> pure ()
        NewProfile -> putStrLn "Creating a new profile."
        StartProfile name -> putStrLn $ printf "Starting profile %s." name

-- Returns the name of the folder or file represented by the file path, without the leading path to it.
baseName :: FP.FilePath -> Text
baseName = encodeString >>> T.pack >>> T.split (\x -> x == '/' || x == '\\') >>> last

-- Picks a task and returns it.
pickTask :: FP.FilePath -> IO Task
pickTask basePath = do
    subDirs <- listDirectory basePath <&> filter (\x -> baseName x & T.isPrefixOf "." & not) <&> zip [1..]
    subDirs & forM_ $ \(idx, sd) -> do
        putStrLn $ printf "%d: %s" idx (baseName sd)
    readTaskFromInput subDirs

-- Reads a task from the console, if a task could not be parsed from the input, the task is asked again.
readTaskFromInput :: [ (Int, FP.FilePath) ] ->  IO Task
readTaskFromInput subDirs = do
    putStrLn "Please choose a profile ('new' to create a new one, 'q' to quit)"
    task <- parseTask subDirs <$> getLine
    maybe (readTaskFromInput subDirs) pure task

-- Parses a string into a task. If a task can be parsed, it is returned, otherwise Nothing is returned.
parseTask :: [ (Int, FP.FilePath) ] -> String -> Maybe Task
parseTask subDirs "q" = Just Quit
parseTask subDirs "new" = Just NewProfile
parseTask subDirs other = number >>= (\n -> find (fst >>> (== n)) subDirs) <&> (snd >>> baseName >>> StartProfile)
    where number = readMaybe other :: Maybe Int

-- Determines the base path for all vscode profiles. This is the subdirectory .code-profiles in the user's home
-- directory.
getBasePath :: IO FP.FilePath
getBasePath = getHomeDirectory <&> flip FP.append (fromText ".code-profiles")

-- Checks whether the base path exists. If it doesn't, it is created.
ensureBasePathExists :: FP.FilePath -> IO ()
ensureBasePathExists basePath = do
    isDir <- isDirectory basePath
    if not isDir then do
        putStrLn "Creating base path"
        createTree basePath
    else pure ()