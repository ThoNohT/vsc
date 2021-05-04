module FileSystem (baseName, getBasePath) where

import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import Filesystem (getHomeDirectory)
import qualified Filesystem.Path.CurrentOS as FP (FilePath, append, encodeString, fromText)

-- Returns the name of the folder or file represented by the file path, without the leading path to it.
baseName :: FP.FilePath -> Text
baseName = FP.encodeString >>> T.pack >>> T.split (\x -> x == '/' || x == '\\') >>> last

-- Determines the base path for all vscode profiles. This is the subdirectory .code-profiles in the user's home
-- directory.
getBasePath :: IO FP.FilePath
getBasePath = getHomeDirectory <&> flip FP.append (FP.fromText ".code-profiles")