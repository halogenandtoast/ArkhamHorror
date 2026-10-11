{-# LANGUAGE NoImplicitPrelude #-}

{- | arkham.build's OAuth 2.0 authorization-code API, as a confidential client.

Docs: https://github.com/arkham-build/arkham.build/blob/main/docs/oauth-integration.md

The client secret is only ever sent from here, in a form body, to the token and
revoke endpoints. Nothing in this module touches the database or a 'Handler', so
the policy around a connection -- who owns it, when to refresh it -- lives with
the rows in "Api.Handler.Arkham.BuildConnection".
-}
module ArkhamBuild.OAuth (
  Config (..),
  Tokens (..),
  Profile (..),
  Failure (..),
  profileRead,
  decksRead,
  decksWrite,
  decksDelete,
  requestedScopes,
  initialRefreshLifetime,
  rotatedRefreshLifetime,
  authorizeUrl,
  exchangeCode,
  refreshTokens,
  revokeRefreshToken,
  fetchProfile,
  ManifestEntry (..),
  DeckManifest (..),
  batchLimit,
  fetchDeckManifest,
  fetchDeckBatch,
) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (NominalDiffTime, nominalDay)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Conduit (
  Request,
  httpLbs,
  parseRequest,
  requestHeaders,
  responseBody,
  responseStatus,
  urlEncodedBody,
 )
import Network.HTTP.Conduit qualified as HTTP
import Network.HTTP.Types (renderSimpleQuery, statusCode)
import Relude
import UnliftIO.Exception (try)

data Config = Config
  { apiBase :: Text
  , clientId :: Text
  , clientSecret :: Text
  , redirectUri :: Text
  }
  deriving stock Show

data Tokens = Tokens
  { accessToken :: Text
  , refreshToken :: Text
  , scope :: Text
  , expiresIn :: Int
  }
  deriving stock Show

instance FromJSON Tokens where
  parseJSON = withObject "Tokens" \o ->
    Tokens
      <$> o
      .: "access_token"
      <*> o
      .: "refresh_token"
      <*> o
      .:? "scope"
      .!= ""
      <*> o
      .:? "expires_in"
      .!= 3600

data Profile = Profile
  { accountId :: Text
  , username :: Text
  }
  deriving stock Show

instance FromJSON Profile where
  parseJSON = withObject "Profile" \o -> Profile <$> o .: "id" <*> o .: "username"

-- | The OAuth endpoints say @error_description@, the user endpoints @message@.
data Failure = Failure
  { status :: Int
  , code :: Text
  , description :: Text
  }
  deriving stock Show

profileRead :: Text
profileRead = "profile:read"

decksRead :: Text
decksRead = "decks:read"

decksWrite :: Text
decksWrite = "decks:write"

{- | Not requested. Nothing here has a reason to destroy a player's decks, and it
is the one scope that could.
-}
decksDelete :: Text
decksDelete = "decks:delete"

{- | @profile:read@ is required of every authorization request. @decks:write@
implies @decks:read@; both are named so the granted list reads the same as what
was asked for.
-}
requestedScopes :: [Text]
requestedScopes = [profileRead, decksRead, decksWrite]

initialRefreshLifetime :: NominalDiffTime
initialRefreshLifetime = 90 * nominalDay

rotatedRefreshLifetime :: NominalDiffTime
rotatedRefreshLifetime = 180 * nominalDay

-- | Where to send the user's browser to start an authorization.
authorizeUrl :: Config -> [Text] -> Text -> Text
authorizeUrl cfg scopes authState =
  cfg.apiBase
    <> "/v2/oauth/authorize"
    <> decodeUtf8
      ( renderSimpleQuery
          True
          [ ("response_type", "code")
          , ("client_id", encodeUtf8 cfg.clientId)
          , ("redirect_uri", encodeUtf8 cfg.redirectUri)
          , ("scope", encodeUtf8 $ T.unwords scopes)
          , ("state", encodeUtf8 authState)
          ]
      )

exchangeCode :: MonadIO m => Config -> Text -> m (Either Failure Tokens)
exchangeCode cfg authCode =
  tokenRequest
    cfg
    [ ("grant_type", "authorization_code")
    , ("code", encodeUtf8 authCode)
    , ("redirect_uri", encodeUtf8 cfg.redirectUri)
    ]

refreshTokens :: MonadIO m => Config -> Text -> m (Either Failure Tokens)
refreshTokens cfg token =
  tokenRequest cfg [("grant_type", "refresh_token"), ("refresh_token", encodeUtf8 token)]

tokenRequest :: MonadIO m => Config -> [(ByteString, ByteString)] -> m (Either Failure Tokens)
tokenRequest cfg params = do
  req <- buildRequest cfg "/v2/oauth/token"
  send $ urlEncodedBody (credentials cfg <> params) req

-- | Revoking a refresh token revokes every access token issued from it.
revokeRefreshToken :: MonadIO m => Config -> Text -> m ()
revokeRefreshToken cfg token = do
  req <- buildRequest cfg "/v2/oauth/revoke"
  let body =
        credentials cfg
          <> [("token", encodeUtf8 token), ("token_type_hint", "refresh_token")]
  void $ perform (urlEncodedBody body req)

fetchProfile :: MonadIO m => Config -> Text -> m (Either Failure Profile)
fetchProfile cfg token = do
  req <- buildRequest cfg "/v2/user/me"
  send $ bearer token req

{- | An id is a string for @account@ and a number for @arkhamdb@, and has meaning
only to its own provider, so it is carried as the JSON it arrived as and sent
back unchanged.
-}
data ManifestEntry = ManifestEntry
  { entrySource :: Text
  , entryId :: Value
  , entryUpdatedAt :: Text
  }
  deriving stock Show

instance FromJSON ManifestEntry where
  parseJSON = withObject "ManifestEntry" \o ->
    ManifestEntry <$> o .: "source" <*> o .: "id" <*> o .:? "updatedAt" .!= ""

data DeckManifest = DeckManifest
  { syncToken :: Maybe Text
  , accountAvailable :: Bool
  , arkhamdbAvailable :: Bool
  , entries :: [ManifestEntry]
  }
  deriving stock Show

instance FromJSON DeckManifest where
  parseJSON = withObject "DeckManifest" \o -> do
    providers <- o .:? "providers" .!= mempty
    DeckManifest
      <$> o
      .:? "arkhamdbSyncToken"
      .!= Nothing
      <*> pure (available providers "account")
      <*> pure (available providers "arkhamdb")
      <*> o
      .:? "decks"
      .!= []
   where
    available providers name = fromMaybe False do
      provider <- KeyMap.lookup name providers
      case fromJSON provider of
        Success (o :: Map Text Bool) -> Map.lookup "available" o
        Error _ -> Nothing

-- | How many targets one batch may carry.
batchLimit :: Int
batchLimit = 250

fetchDeckManifest :: MonadIO m => Config -> Text -> m (Either Failure DeckManifest)
fetchDeckManifest cfg token = do
  req <- buildRequest cfg "/v2/user/decks/manifest"
  send $ bearer token req

{- | The decks come back as the raw objects arkham.build serves, in the order
asked for. Decoding them into a decklist is the caller's business; this module
does not know what a card is.
-}
fetchDeckBatch
  :: MonadIO m => Config -> Text -> Maybe Text -> [ManifestEntry] -> m (Either Failure [Value])
fetchDeckBatch cfg token token' targets = do
  req <- buildRequest cfg "/v2/user/decks/batch"
  let body =
        object
          [ "arkhamdbSyncToken" .= token'
          , "decks" .= [object ["source" .= entrySource e, "id" .= entryId e] | e <- targets]
          ]
  fmap (fmap decks) $ send $ bearer token $ jsonBody body req

newtype BatchResponse = BatchResponse {decks :: [Value]}

instance FromJSON BatchResponse where
  parseJSON = withObject "BatchResponse" \o -> BatchResponse <$> o .:? "decks" .!= []

jsonBody :: Value -> Request -> Request
jsonBody value req =
  req
    { HTTP.method = "POST"
    , HTTP.requestBody = HTTP.RequestBodyLBS (encode value)
    , requestHeaders = ("Content-Type", "application/json") : requestHeaders req
    }

credentials :: Config -> [(ByteString, ByteString)]
credentials cfg =
  [("client_id", encodeUtf8 cfg.clientId), ("client_secret", encodeUtf8 cfg.clientSecret)]

bearer :: Text -> Request -> Request
bearer token req =
  req {requestHeaders = ("Authorization", "Bearer " <> encodeUtf8 token) : requestHeaders req}

-- | @checkResponse@ cleared, so a 4xx is a response to read rather than a throw.
buildRequest :: MonadIO m => Config -> Text -> m Request
buildRequest cfg path = do
  req <- liftIO $ parseRequest $ T.unpack $ cfg.apiBase <> path
  pure
    req
      { HTTP.checkResponse = \_ _ -> pure ()
      , HTTP.responseTimeout = HTTP.responseTimeoutMicro 15_000_000
      , requestHeaders = ("Accept", "application/json") : requestHeaders req
      }

send :: (FromJSON a, MonadIO m) => Request -> m (Either Failure a)
send req =
  perform req >>= \case
    Left err -> pure $ Left err
    Right (code', body) -> pure case eitherDecode body of
      Right value -> Right value
      Left _ -> Left $ failureFrom code' body

perform :: MonadIO m => Request -> m (Either Failure (Int, BSL.ByteString))
perform req = do
  manager <- liftIO getGlobalManager
  liftIO (try @IO @SomeException (httpLbs req manager)) >>= \case
    Left _ -> pure $ Left $ Failure 0 "unreachable" "Could not reach arkham.build"
    Right response -> do
      let code' = statusCode $ responseStatus response
          body = responseBody response
      pure
        $ if code' >= 200 && code' < 300
          then Right (code', body)
          else Left $ failureFrom code' body

failureFrom :: Int -> BSL.ByteString -> Failure
failureFrom code' body = case decode body of
  Just (ErrorBody mCode mDescription mMessage) ->
    Failure
      code'
      (fromMaybe "unexpected_response" mCode)
      (fromMaybe (fromMaybe (snippet body) mMessage) mDescription)
  Nothing -> Failure code' "unexpected_response" (snippet body)
 where
  snippet = T.strip . decodeUtf8 . BSL.toStrict . BSL.take 500

data ErrorBody = ErrorBody (Maybe Text) (Maybe Text) (Maybe Text)

instance FromJSON ErrorBody where
  parseJSON = withObject "ErrorBody" \o ->
    ErrorBody <$> o .:? "error" <*> o .:? "error_description" <*> o .:? "message"
