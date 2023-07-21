module Base.Api.Handler.Authentication where

import Crypto.BCrypt
import Data.Text.Encoding qualified as TE
import Import
import Types

authenticationToUser :: Authentication -> Handler (Maybe (Entity User))
authenticationToUser Authentication {..} = do
  muser <- runDB $ getBy $ UniqueEmail authenticationEmail
  case muser of
    Nothing -> pure Nothing
    Just entity@(Entity _ user) ->
      if validatePassword
        (p $ userPasswordDigest user)
        (p authenticationPassword)
        then pure $ Just entity
        else pure Nothing
 where
  p = TE.encodeUtf8

postApiV1AuthenticationR :: Handler Token
postApiV1AuthenticationR = do
  authentication <- requireCheckJsonBody
  muser <- authenticationToUser authentication
  case muser of
    Nothing -> notAuthenticated
    Just (Entity userId _) -> Token <$> userIdToToken userId
