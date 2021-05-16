module Console (request, requestM, request_) where

import Control.Arrow ((>>>))
import Control.Monad.IO.Class (MonadIO (liftIO))
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

-- Same as request, but the parser may return an IO-compatible monad.
requestM :: forall a m. MonadIO m => Maybe String -> (String -> m (Either (Maybe String) a)) -> m a
requestM msgBefore parser = do
    liftIO $ sequence_ $ putStrLn <$> msgBefore
    getInput
  where
    getInput = liftIO getLine >>= parser >>= either handleError pure
    handleError mErr = do
        liftIO $ sequence_ $ putStrLn <$> (mErr <|> msgBefore)
        getInput
