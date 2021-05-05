module Task.NewProfile (runNewProfile) where

import Console (request_)
import Control.Arrow ((>>>))
import Data.Bifunctor (first)
import Data.Either.Combinators (rightToMaybe)
import Text.Parsec (ParseError, digit, eof, letter, many, oneOf, parse, (<|>))

runNewProfile :: IO ()
runNewProfile = do
    newName <-
        request_
            (Just "Please provide a name for the new profile")
            (Just "Invalid profile name. Name must start with a letter and can only contain letters, digits and _, -, ~.")
            parseProfileName
    putStrLn "Jo"

-- Checks whether the specified name is a valid profile name.
parseProfileName :: String -> Maybe String
parseProfileName = parse nameParser "" >>> rightToMaybe
  where
    nameParser = do
        first <- letter
        remaining <- many (letter <|> digit <|> oneOf "_-~")
        eof
        return $ first : remaining