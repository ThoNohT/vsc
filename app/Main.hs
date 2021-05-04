module Main where

import Control.Arrow ((>>>))
import Control.Monad (forM_)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Filesystem (createTree, getHomeDirectory, isDirectory, listDirectory)
import Filesystem.Path.CurrentOS as FP (FilePath, append, encodeString, fromText)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Task
    = NewProfile
    | Quit
    | StartProfile Text

-- The runtime used in this application, providing read access to the base path.
type VscRuntime = ReaderT FP.FilePath IO

main :: IO ()
main = do
    basePath <- getBasePath

    flip runReaderT basePath $ do
        ensureBasePathExists
        task <- pickTask

        liftIO $ case task of
            Quit -> pure ()
            NewProfile -> putStrLn "Creating a new profile."
            StartProfile name -> putStrLn $ printf "Starting profile %s." name

-- Picks a task and returns it.
pickTask :: VscRuntime Task
pickTask = do
    basePath <- ask
    liftIO $ do
        subDirs <- listDirectory basePath <&> filter (not . T.isPrefixOf "." . baseName) <&> zip [1 ..]
        subDirs & forM_ $ \(idx, sd) -> putStrLn $ printf "%d: %s" idx (baseName sd)
        readTaskFromInput subDirs

-- Reads a task from the console, if a task could not be parsed from the input, the task is asked again.
readTaskFromInput :: [(Int, FP.FilePath)] -> IO Task
readTaskFromInput subDirs = do
    putStrLn "Please choose a profile ('new' to create a new one, 'q' to quit)"
    task <- parseTask subDirs <$> getLine
    maybe (readTaskFromInput subDirs) pure task

-- Parses a string into a task. If a task can be parsed, it is returned, otherwise Nothing is returned.
parseTask :: [(Int, FP.FilePath)] -> String -> Maybe Task
parseTask subDirs "q" = Just Quit
parseTask subDirs "new" = Just NewProfile
parseTask subDirs other = number >>= (\n -> find (fst >>> (== n)) subDirs) <&> (snd >>> baseName >>> StartProfile)
  where
    number = readMaybe other :: Maybe Int

-- Determines the base path for all vscode profiles. This is the subdirectory .code-profiles in the user's home
-- directory.
getBasePath :: IO FP.FilePath
getBasePath = getHomeDirectory <&> flip FP.append (fromText ".code-profiles")

-- Checks whether the base path exists. If it doesn't, it is created.
ensureBasePathExists :: VscRuntime ()
ensureBasePathExists = do
    basePath <- ask
    liftIO $ do
        isDir <- isDirectory basePath
        if not isDir
            then do
                putStrLn "Creating base path"
                createTree basePath
            else pure ()

-- Returns the name of the folder or file represented by the file path, without the leading path to it.
baseName :: FP.FilePath -> Text
baseName = encodeString >>> T.pack >>> T.split (\x -> x == '/' || x == '\\') >>> last