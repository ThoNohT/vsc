module Main where

import Control.Monad (forM_)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT))
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Text as T
import FileSystem (baseName, getBasePath)
import Filesystem (createTree, isDirectory, listDirectory)
import Filesystem.Path.CurrentOS as FP (FilePath)
import Task (Task (..), readTaskFromInput)
import Text.Printf (printf)

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