module Task (Task (..), pickTask, runTask) where

import Console (readLine, requestM, request_)
import Control.Monad (forM_)
import Data.Either (isRight)
import Data.Either.Combinators (rightToMaybe)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (toList)
import Data.Map.Strict as Map (Map, fromList, lookup)
import Data.Maybe (fromMaybe)
import Data.Text as T (Text, isPrefixOf)
import Environment (ProfileType (..), baseName, getBasePath, subdir, typeName)
import Filesystem (listDirectory)
import qualified Filesystem.Path.CurrentOS as FP (FilePath)
import GHC.Exts (sortWith)
import NewProfile (NewProfile, getNewProfile, newProfileName)
import Profile (Profile)
import qualified Profile
import Text.Parsec (char, digit, eof, many, oneOf, parse, (<|>))
import Text.Parsec.Char (letter)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Task
    = Manage
    | StartProfile Profile
    | -- Manage profiles
      StartTemplate
    | NewProfile
    | NewTemplate
    | DeleteProfile ProfileType

-- Parses a number corresponding to a profile in the provided map. If no number matches, Nothing is returned.
parseProfileNumber :: Map Int Profile -> String -> Maybe Profile
parseProfileNumber profiles text = readMaybe text >>= (`Map.lookup` profiles)

-- Parses a template that can be used to copy into a profile, or the input 'n' that means
-- no template should be copied.
parseTemplateForProfile :: Map Int Profile -> String -> Maybe (Either () Profile)
parseTemplateForProfile _ "n" = Just $ Left ()
parseTemplateForProfile profiles text = parseProfileNumber profiles text <&> Right

-- Parses a string into a task. If a task can be parsed, it is returned, otherwise Nothing is returned.
parseTask :: Map Int Profile -> String -> Maybe Task
parseTask _ "m" = Just Manage
parseTask profiles other = parseProfileNumber profiles other <&> StartProfile

-- parses a string into a task for managing profiles and templates. If a task can be parsed, it is returned, otherwise
-- Nothing is returned.
parseManageTask :: String -> Maybe Task
parseManageTask "np" = Just NewProfile
parseManageTask "dp" = Just $ DeleteProfile ProfileType
parseManageTask "nt" = Just NewTemplate
parseManageTask "dt" = Just $ DeleteProfile TemplateType
parseManageTask "st" = Just StartTemplate
parseManageTask _ = Nothing

-- Picks a task and returns it.
pickTask :: FP.FilePath -> IO Task
pickTask basePath = do
    profiles <- Profile.list basePath ProfileType <&> zip [1 ..]
    let profileMap = fromList profiles
    profiles & forM_ $ \(idx, p) -> putStrLn $ printf "%d: %s" idx (Profile.profileName p)
    request_ (Just "Please choose a profile ('m' to manage profiles)") Nothing (parseTask profileMap)

-- Picks a profile and returns it.
pickProfile :: FP.FilePath -> ProfileType -> IO Profile
pickProfile basePath pType = do
    templates <- Profile.list basePath pType <&> zip [1 ..]
    let templateMap = fromList templates
    templates & forM_ $ \(idx, t) -> putStrLn $ printf "%d: %s" idx (Profile.profileName t)
    request_ (Just $ printf "Please choose a %s" $ typeName pType) Nothing (parseProfileNumber templateMap)

-- Picks a task from the list of manage tasks, and returns it.
pickManageTask :: FP.FilePath -> IO Task
pickManageTask basePath = do
    request_
        ( Just $
            unlines
                [ "np: New profile"
                , "dp: Delete profile"
                , "nt: New template"
                , "dt: Delete template"
                , "st: Start template"
                ]
        )
        Nothing
        parseManageTask

pickTemplateToCopy :: FP.FilePath -> IO (Maybe Profile)
pickTemplateToCopy basePath = do
    templates <- Profile.list basePath TemplateType <&> zip [1 ..]
    let templateMap = fromList templates
    templates & forM_ $ \(idx, t) -> putStrLn $ printf "%d: %s" idx (Profile.profileName t)
    rightToMaybe
        <$> request_
            (Just "Copy from a template? (n to start with empty profile)")
            Nothing
            (parseTemplateForProfile templateMap)

-- Runs a task.
runTask :: FP.FilePath -> Task -> IO ()
runTask basePath NewTemplate =
    requestM (Just "Please provide a name for the new template") (getNewProfile basePath TemplateType)
        >>= readLine (printf "Press Enter to confirm: create template %s." . newProfileName)
        >>= Profile.create Nothing
        >>= Profile.start
runTask basePath NewProfile = do
    name <- requestM (Just "Please provide a name for the new profile") (getNewProfile basePath ProfileType)
    source <-
        pickTemplateToCopy basePath
            >>= readLine
                ( printf "Press Enter to confirm: make a new profile %s from %s." (newProfileName name)
                    . maybe "scratch" Profile.profileName
                )
    Profile.create source name >>= Profile.start
runTask basePath (StartProfile profile) = Profile.start profile
runTask basePath Manage = pickManageTask basePath >>= runTask basePath
runTask basePath StartTemplate = pickProfile basePath TemplateType >>= runTask basePath . StartProfile
runTask basePath (DeleteProfile pType) =
    pickProfile basePath pType
        >>= readLine
            ( \p ->
                printf
                    "Press Enter to confirm: delete the %s %s."
                    (typeName $ Profile.profileType p)
                    (Profile.profileName p)
            )
        >>= Profile.delete
        >>= restart basePath

-- Restarts picking and running a task after a task is completed.
-- Should only be used by tasks that don't actually start VS Code.
restart :: FP.FilePath -> () -> IO ()
restart basePath _ = pickTask basePath >>= runTask basePath