module Profile (Profile, create, delete, list, profileName, profileType, start) where

import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T (isPrefixOf, pack)
import Environment (ProfileType, baseName, subdir, typeName)
import Filesystem (copyFile, createTree, isDirectory, listDirectory, removeTree)
import qualified Filesystem.Path.CurrentOS as FP (FilePath, append, encodeString, fromText)
import NewProfile (NewProfile (..))
import Path.IO (copyDirRecur)
import Path.Internal (Path (Path))
import qualified System.Process as Proc
import Text.Printf (printf)

-- Public

-- A profile that definitely exists.
data Profile = MkProfile ProfileType FP.FilePath

-- Creates a profile from a new profile, returns the existing profile.
create :: Maybe Profile -> NewProfile -> IO Profile
create copySource (MkNewProfile pType newPath) = do
    createTree newPath
    case copySource of
        Just (MkProfile _ sourcePath) ->
            copyDirRecur (Path $ FP.encodeString sourcePath) (Path $ FP.encodeString newPath)
        _ -> pure ()

    pure $ MkProfile pType newPath

-- Deletes a profile.
delete :: Profile -> IO ()
delete profile = do
    removeTree $ profilePath profile
    putStrLn $ printf "Deleted %s %s" (typeName . profileType $ profile) (profileName profile)

-- Starts a profile.
start :: Profile -> IO ()
start profile@(MkProfile _ path) =
    let extsPath = FP.encodeString $ FP.append path (FP.fromText "exts")
        dataPath = FP.encodeString $ FP.append path (FP.fromText "data")
        command = printf "code --extensions-dir %s --user-data-dir %s ." extsPath dataPath
     in do
            putStrLn $
                printf "Starting %s %s" (typeName . profileType $ profile) (profileName profile)
            _ <- Proc.runCommand command
            pure ()

-- Lists all profiles in the sub-directory of a path.
list :: FP.FilePath -> ProfileType -> IO [Profile]
list path pType =
    listDirectory (FP.append path (subdir pType))
        <&> filter (not . T.isPrefixOf "." . baseName)
        <&> sort
        <&> map (MkProfile pType)

-- Retrieves the name of a profile.
profileName :: Profile -> Text
profileName = profilePath >>> baseName

-- Retrieves the type of a profile.
profileType :: Profile -> ProfileType
profileType (MkProfile t _) = t

-- Private

-- Extract the path from a profile.
profilePath :: Profile -> FP.FilePath
profilePath (MkProfile _ p) = p
