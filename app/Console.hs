module Console (request, request_) where

import Control.Arrow ((>>>))
import Data.Either.Combinators (maybeToRight)
import Data.Function ((&))
import Data.Functor ((<&>))
import GHC.Base ((<|>))

-- Request a value from the user by reading it from the console. If specified, a message is
-- displayed before asking the first time. If parsing the input failed, the error message is shown
-- and the user is asked for the input again until they provide a valid string. If no error message is returned
-- the message before is displayed again (if it is defined).
request :: forall a. Maybe String -> (String -> Either (Maybe String) a) -> IO a
request msgBefore parser = do
    sequence_ $ putStrLn <$> msgBefore
    getInput
  where
    getInput = getLine >>= (parser >>> either handleError pure)
    handleError mErr = do
        sequence_ $ putStrLn <$> (mErr <|> msgBefore)
        getInput

-- Same as request, but the error message cannot be dependent on the parser.
request_ :: forall a. Maybe String -> Maybe String -> (String -> Maybe a) -> IO a
request_ msgBefore errMsg parser = request msgBefore (parser >>> maybeToRight errMsg)