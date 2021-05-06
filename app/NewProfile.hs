module NewProfile (NewProfile (..), getNewProfile) where

import Control.Arrow ((>>>))
import Data.Either.Combinators (rightToMaybe)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Environment (baseName)
import Filesystem (isDirectory)
import qualified Filesystem.Path.CurrentOS as FP (FilePath, append, fromText)
import Text.Parsec (ParseError, digit, eof, letter, many, oneOf, parse, (<|>))

-- Public

-- A new profile that may not exist.
data NewProfile
    = MkNewProfile FP.FilePath
    | MkNewTemplate FP.FilePath

-- Returns a new profile with the specified name. Succeeds if the name is a valid profile name, and if the profile
-- does not yet exist in the specified path. The profile is not yet created.
getNewProfile :: FP.FilePath -> String -> IO (Either (Maybe String) NewProfile)
getNewProfile path name =
    -- Check that the name is valid.
    case parseNewProfileName name of
        Nothing ->
            let error =
                    "Invalid profile name. Name must start with a "
                        ++ "letter and can only contain letters, digits and _, -, ~."
             in pure $ Left $ Just error
        Just p -> do
            -- Check that the directory doesn't yet exist.
            let newProfile = mkNewProfileInPath path p
            profileExists <- exists newProfile
            pure $
                if profileExists
                    then Left $ Just "A profile with this name already exists."
                    else Right newProfile

-- Private

-- Creates a new profile with the specified name in the specified path.
mkNewProfileInPath :: FP.FilePath -> Text -> NewProfile
mkNewProfileInPath basePath = FP.fromText >>> FP.append basePath >>> MkNewProfile

-- Creates a new template with the specified name in the specified path.
mkNewTemplateInPath :: FP.FilePath -> Text -> NewProfile
mkNewTemplateInPath basePath = FP.fromText >>> FP.append basePath >>> MkNewTemplate

-- Extract the path from a new profile.
newProfilePath :: NewProfile -> FP.FilePath
newProfilePath (MkNewProfile p) = p
newProfilePath (MkNewTemplate p) = p

-- Retrieves the name of a new profile.
newProfileName :: NewProfile -> Text
newProfileName = newProfilePath >>> baseName

-- Checks whether a new profile exists.
exists :: NewProfile -> IO Bool
exists = newProfilePath >>> isDirectory

-- parses the name for a new profile.
parseNewProfileName :: String -> Maybe Text
parseNewProfileName name =
    parse nameParser "" name & rightToMaybe <&> T.pack
  where
    nameParser = do
        first <- letter
        remaining <- many (letter <|> digit <|> oneOf "_-~")
        eof
        return $ first : remaining