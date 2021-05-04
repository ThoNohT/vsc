module Environment (baseName, ensureBasePathExists, getBasePath) where

import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import Filesystem (getHomeDirectory, isDirectory, createTree)
import qualified Filesystem.Path.CurrentOS as FP (FilePath, append, encodeString, fromText)

-- Returns the name of the folder or file represented by the file path, without the leading path to it.
baseName :: FP.FilePath -> Text
baseName = FP.encodeString >>> T.pack >>> T.split (\x -> x == '/' || x == '\\') >>> last

-- Determines the base path for all vscode profiles. This is the subdirectory .code-profiles in the user's home
-- directory.
getBasePath :: IO FP.FilePath
getBasePath = getHomeDirectory <&> flip FP.append (FP.fromText ".code-profiles")

-- Checks whether the base path exists. If it doesn't, it is created.
ensureBasePathExists :: FP.FilePath -> IO ()
ensureBasePathExists basePath = do
    isDir <- isDirectory basePath
    if not isDir
        then do
            putStrLn "Creating base path"
            createTree basePath
        else pure ()