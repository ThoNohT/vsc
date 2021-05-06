module Profile (Profile, create, list, name, start) where

import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T (isPrefixOf, pack)
import Environment (baseName)
import Filesystem (createTree, isDirectory, listDirectory)
import qualified Filesystem.Path.CurrentOS as FP (FilePath, append, encodeString, fromText)
import NewProfile (NewProfile (..))
import qualified System.Process as Proc
import Text.Printf (printf)

-- Public

-- A profile that definitely exists.
data Profile
    = MkProfile FP.FilePath
    | MkTemplate FP.FilePath

-- Creates a profile from a new profile, returns the existing profile.
create :: NewProfile -> IO Profile
create (MkNewProfile path) = do
    createTree path
    pure $ MkProfile path
create (MkNewTemplate path) = do
    putStrLn "Templates are not yet supported."
    pure undefined

-- Starts a profile.
start :: Profile -> IO ()
start profile@(MkProfile path) =
    let extsPath = FP.encodeString $ FP.append path (FP.fromText "exts")
        dataPath = FP.encodeString $ FP.append path (FP.fromText "data")
        command = printf "code --extensions-dir %s --user-data-dir %s ." extsPath dataPath
     in do
            putStrLn $ printf "Starting profile %s" $ name profile
            putStrLn command
            _ <- Proc.createProcess $ Proc.shell $ command
            pure ()
start (MkTemplate p) = putStrLn "Templates are not yet supported."

-- Lists all profiles in a path.
list :: FP.FilePath -> IO [Profile]
list path =
    listDirectory path
        <&> filter (not . T.isPrefixOf "." . baseName)
        <&> sort
        <&> map MkProfile

-- Retrieves the name of a profile.
name :: Profile -> Text
name = profilePath >>> baseName

-- Private

-- Extract the path from a profile.
profilePath :: Profile -> FP.FilePath
profilePath (MkProfile p) = p
profilePath (MkTemplate p) = p
