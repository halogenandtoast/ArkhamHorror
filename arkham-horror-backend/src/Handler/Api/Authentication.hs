module Handler.Api.Authentication where

import           Arkham.Types
import           Crypto.BCrypt
import qualified Data.Text.Encoding as TE
import           Import

data Authentication = Authentication
    { email    :: Text
    , password :: Text
    }
    deriving stock (Generic)

instance FromJSON Authentication

authenticationToUser :: Authentication -> Handler (Maybe (Entity User))
authenticationToUser Authentication {..} = do
  muser <- runDB $ getBy $ UniqueEmail email
  case muser of
    Nothing -> pure Nothing
    Just entity@(Entity _ user) ->
      if validatePassword (p $ userPasswordDigest user) (p password)
        then pure $ Just entity
        else pure Nothing
  where p = TE.encodeUtf8

postApiV1AuthenticationR :: Handler Token
postApiV1AuthenticationR = do
  authentication <- requireCheckJsonBody
  muser          <- authenticationToUser authentication
  case muser of
    Nothing                -> notAuthenticated
    Just (Entity userId _) -> Token <$> userIdToToken userId
