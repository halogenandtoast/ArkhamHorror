{-# LANGUAGE NoImplicitPrelude #-}

{- | arkham.build's OAuth 2.0 authorization-code API, as a confidential client.

Docs: https://github.com/arkham-build/arkham.build/blob/main/docs/oauth-integration.md
Spec: https://github.com/arkham-build/arkham.build/blob/main/docs/openapi/oauth-user-api.json

The client secret is only ever sent from here, in a form body, to the token and
revoke endpoints. Nothing in this module touches the database or a 'Handler', so
the policy around a connection -- who owns it, when to refresh it -- lives with
the rows in "Api.Handler.Arkham.BuildConnection".

The API offers optional PKCE. It is not used: PKCE protects a public client that
cannot hold a secret, and the docs are explicit that it does not replace
confidential-client authentication. Adopting it would mean storing a verifier
per authorization request for no gain here.
-}
module ArkhamBuild.OAuth (
  Config (..),
  Tokens (..),
  Profile (..),
  Failure (..),
  rateLimited,
  notFound,
  conflict,
  upstreamUnavailable,
  insufficientScope,
  accountBanned,
  profileRead,
  decksRead,
  decksWrite,
  decksDelete,
  requestedScopes,
  refreshLifetime,
  authorizeUrl,
  exchangeCode,
  refreshTokens,
  revokeRefreshToken,
  fetchProfile,
  accountSource,
  arkhamdbSource,
  DeckRef (..),
  entryRef,
  ManifestEntry (..),
  DeckManifest (..),
  batchLimit,
  fetchDeckManifest,
  fetchDeckBatch,
  Deck (..),
  fetchDeck,
  DeckWrite,
  deckWrite,
  newDeckWrite,
  withName,
  withInvestigatorCode,
  withInvestigatorName,
  withSlots,
  withSideSlots,
  withIgnoreDeckLimitSlots,
  withMeta,
  withTags,
  withDescription,
  withExileString,
  withTabooId,
  withXp,
  withXpAdjustment,
  withXpSpent,
  createDeck,
  replaceDeck,
  upgradeDeck,
) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isDigit)
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
import Network.HTTP.Types (renderSimpleQuery, statusCode, urlEncode)
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

{- | The shared limit is 100 matching requests per 60 seconds per client IP, and
blocks for a further 60 once tripped. Every player shares one egress IP in
production, so this is a per-deployment budget rather than a per-player one.
-}
rateLimited :: Failure -> Bool
rateLimited failure = failure.status == 429

notFound :: Failure -> Bool
notFound failure = failure.status == 404 || failure.code == "not_found"

-- | An invalid history transition: an upgrade of a deck that already has one.
conflict :: Failure -> Bool
conflict failure = failure.status == 409 || failure.code == "conflict"

{- | ArkhamDB is a second upstream that fails on its own. A manifest degrades to
account decks only; a single-deck read or write 503s.
-}
upstreamUnavailable :: Failure -> Bool
upstreamUnavailable failure =
  failure.status == 503 || failure.code == "upstream_unavailable"

-- | A 403 the player fixes by reconnecting, unlike 'accountBanned'.
insufficientScope :: Failure -> Bool
insufficientScope failure = failure.code == "insufficient_scope"

accountBanned :: Failure -> Bool
accountBanned failure = failure.code == "account_banned"

profileRead :: Text
profileRead = "profile:read"

decksRead :: Text
decksRead = "decks:read"

decksWrite :: Text
decksWrite = "decks:write"

{- | Not requested. Nothing here has a reason to destroy a player's decks, and it
is the one scope that could -- @?all=true@ takes the whole history chain with it.
-}
decksDelete :: Text
decksDelete = "decks:delete"

{- | @profile:read@ is required of every authorization request. @decks:write@
implies @decks:read@; both are named so the granted list reads the same as what
was asked for.
-}
requestedScopes :: [Text]
requestedScopes = [profileRead, decksRead, decksWrite]

{- | A refresh token is good for 180 days, both as issued and after each
rotation.
-}
refreshLifetime :: NominalDiffTime
refreshLifetime = 180 * nominalDay

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

-- | arkham.build's own deck storage. Always available.
accountSource :: Text
accountSource = "account"

-- | Needs a linked ArkhamDB identity, and ArkhamDB itself to be up.
arkhamdbSource :: Text
arkhamdbSource = "arkhamdb"

{- | Everything needed to address one deck.

An id is a string for @account@ and a number for @arkhamdb@, and has meaning
only to its own provider, so it is carried as the JSON it arrived as and sent
back unchanged.
-}
data DeckRef = DeckRef
  { refSource :: Text
  , refId :: Value
  }
  deriving stock (Show, Eq)

data ManifestEntry = ManifestEntry
  { entrySource :: Text
  , entryId :: Value
  , entryUpdatedAt :: Text
  , entryVersion :: Text
  {- ^ Changes whenever the deck does, so two manifests can be compared without
  reading either deck.
  -}
  }
  deriving stock Show

entryRef :: ManifestEntry -> DeckRef
entryRef entry = DeckRef entry.entrySource entry.entryId

instance FromJSON ManifestEntry where
  parseJSON = withObject "ManifestEntry" \o ->
    ManifestEntry
      <$> o
      .: "source"
      <*> o
      .: "id"
      <*> o
      .:? "updatedAt"
      .!= ""
      <*> o
      .:? "version"
      .!= ""

data DeckManifest = DeckManifest
  { manifestVersion :: Text
  -- ^ Changes when any deck's source, id, version or update time changes.
  , syncToken :: Maybe Text
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
      .:? "version"
      .!= ""
      <*> o
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

{- | Pass a source to list only that provider's decks. A manifest that includes
ArkhamDB does a conditional sync upstream, so asking only for @account@ is both
cheaper and immune to ArkhamDB being down.
-}
fetchDeckManifest
  :: MonadIO m => Config -> Text -> Maybe Text -> m (Either Failure DeckManifest)
fetchDeckManifest cfg token mSource = do
  req <- buildRequest cfg $ "/v2/user/decks/manifest" <> query
  send $ bearer token req
 where
  query = case mSource of
    Nothing -> ""
    Just source -> "?source=" <> source

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
  fmap (fmap decks) $ send $ bearer token $ jsonBody "POST" body req

newtype BatchResponse = BatchResponse {decks :: [Value]}

instance FromJSON BatchResponse where
  parseJSON = withObject "BatchResponse" \o -> BatchResponse <$> o .:? "decks" .!= []

{- | A deck as arkham.build serves it.

@deckRaw@ is the object it arrived as, and is what a write sends back -- see
'deckWrite'.

The typed fields are forgiving where the spec is strict: a deck missing a
@tags@ or @description_md@ it is supposed to have should still be readable,
since the raw object is the authority for any write.
-}
data Deck = Deck
  { deckId :: Value
  , deckSource :: Maybe Text
  , deckName :: Text
  , deckInvestigatorCode :: Text
  , deckInvestigatorName :: Maybe Text
  , deckSlots :: Map Text Int
  , deckSideSlots :: Maybe (Map Text Int)
  , deckIgnoreDeckLimitSlots :: Maybe (Map Text Int)
  , deckMeta :: Text
  , deckTags :: Text
  , deckDescriptionMd :: Text
  , deckExileString :: Maybe Text
  , deckTabooId :: Maybe Int
  , deckXp :: Maybe Int
  -- ^ Total experience available to the deck, not the amount still unspent.
  , deckXpAdjustment :: Maybe Int
  , deckXpSpent :: Maybe Int
  , deckProblem :: Maybe Text
  , deckVersion :: Text
  , deckPreviousDeck :: Maybe Value
  , deckNextDeck :: Maybe Value
  , deckUserId :: Maybe Int
  , deckDateCreation :: Text
  , deckDateUpdate :: Text
  , deckRaw :: Object
  }
  deriving stock Show

instance FromJSON Deck where
  parseJSON = withObject "Deck" \o -> do
    deckId <- o .: "id"
    deckSource <- o .:? "source"
    deckName <- o .: "name"
    deckInvestigatorCode <- o .: "investigator_code"
    deckInvestigatorName <- o .:? "investigator_name"
    deckSlots <- o .: "slots"
    deckSideSlots <- o .:? "sideSlots"
    deckIgnoreDeckLimitSlots <- o .:? "ignoreDeckLimitSlots"
    deckMeta <- o .:? "meta" .!= ""
    deckTags <- o .:? "tags" .!= ""
    deckDescriptionMd <- o .:? "description_md" .!= ""
    deckExileString <- o .:? "exile_string"
    deckTabooId <- o .:? "taboo_id"
    deckXp <- o .:? "xp"
    deckXpAdjustment <- o .:? "xp_adjustment"
    deckXpSpent <- o .:? "xp_spent"
    deckProblem <- o .:? "problem"
    deckVersion <- o .:? "version" .!= ""
    deckPreviousDeck <- o .:? "previous_deck"
    deckNextDeck <- o .:? "next_deck"
    deckUserId <- o .:? "user_id"
    deckDateCreation <- o .:? "date_creation" .!= ""
    deckDateUpdate <- o .:? "date_update" .!= ""
    let deckRaw = o
    pure Deck {..}

fetchDeck :: MonadIO m => Config -> Text -> DeckRef -> m (Either Failure Deck)
fetchDeck cfg token deckRef = withDeckPath deckRef \path -> do
  req <- buildRequest cfg path
  send $ bearer token req

{- | The mutable content of a deck, as @OAuthDeckWrite@.

Held as the deck object rather than a record of the fields we happen to care
about, because a write replaces mutable content wholesale while silently
ignoring the server-owned fields mixed in with it. Sending a record would erase
whatever the record does not model -- the player's description, their tags, a
field arkham.build adds next month. So a write starts from the deck as read
('deckWrite') or from the six required fields ('newDeckWrite'), and the @with@
functions change one key at a time.
-}
newtype DeckWrite = DeckWrite Object
  deriving newtype (Show, ToJSON)

-- | Every mutable field preserved; a write of this changes nothing.
deckWrite :: Deck -> DeckWrite
deckWrite deck = DeckWrite deck.deckRaw

{- | A deck built from nothing: the fields @OAuthDeckWrite@ requires, with the
rest left to arkham.build's defaults.
-}
newDeckWrite :: Text -> Text -> Map Text Int -> DeckWrite
newDeckWrite name investigatorCode slots =
  DeckWrite
    $ KeyMap.fromList
      [ ("name", toJSON name)
      , ("investigator_code", toJSON investigatorCode)
      , ("slots", toJSON slots)
      , ("description_md", String "")
      , ("meta", String "")
      , ("tags", String "")
      ]

setKey :: ToJSON a => Key -> a -> DeckWrite -> DeckWrite
setKey key value (DeckWrite o) = DeckWrite $ KeyMap.insert key (toJSON value) o

withName :: Text -> DeckWrite -> DeckWrite
withName = setKey "name"

-- | An upgrade may not change the investigator.
withInvestigatorCode :: Text -> DeckWrite -> DeckWrite
withInvestigatorCode = setKey "investigator_code"

{- | arkham.build leaves this blank on an account deck, so a deck created from
here is the one case worth filling it in for.
-}
withInvestigatorName :: Maybe Text -> DeckWrite -> DeckWrite
withInvestigatorName = setKey "investigator_name"

withSlots :: Map Text Int -> DeckWrite -> DeckWrite
withSlots = setKey "slots"

withSideSlots :: Map Text Int -> DeckWrite -> DeckWrite
withSideSlots = setKey "sideSlots"

withIgnoreDeckLimitSlots :: Map Text Int -> DeckWrite -> DeckWrite
withIgnoreDeckLimitSlots = setKey "ignoreDeckLimitSlots"

{- | arkham.build's @meta@ is itself a JSON document in a string, and carries
customizations, attachments, the extra deck and the alternate front. Replacing
it wholesale drops all of them, so edit the string rather than rebuilding it.
-}
withMeta :: Text -> DeckWrite -> DeckWrite
withMeta = setKey "meta"

withTags :: Text -> DeckWrite -> DeckWrite
withTags = setKey "tags"

withDescription :: Text -> DeckWrite -> DeckWrite
withDescription = setKey "description_md"

withExileString :: Maybe Text -> DeckWrite -> DeckWrite
withExileString = setKey "exile_string"

withTabooId :: Maybe Int -> DeckWrite -> DeckWrite
withTabooId = setKey "taboo_id"

{- | On an upgrade this is the child's TOTAL experience, and may not be less
than what carries over from the parent.
-}
withXp :: Maybe Int -> DeckWrite -> DeckWrite
withXp = setKey "xp"

withXpAdjustment :: Maybe Int -> DeckWrite -> DeckWrite
withXpAdjustment = setKey "xp_adjustment"

withXpSpent :: Maybe Int -> DeckWrite -> DeckWrite
withXpSpent = setKey "xp_spent"

{- | @problem@ is the remaining mutable field, and deliberately has no setter:
it is arkham.build's own verdict on whether the deck is legal, and ours to read
rather than to assert.
-}

{- | An account create mints a UUID; an ArkhamDB create takes the id ArkhamDB
chooses. Either way the returned deck is the authority on it.
-}
createDeck :: MonadIO m => Config -> Text -> Text -> DeckWrite -> m (Either Failure Deck)
createDeck cfg token source write = do
  req <- buildRequest cfg $ "/v2/user/decks/" <> source
  send $ bearer token $ jsonBody "POST" (toJSON write) req

-- | Replaces all mutable content, keeping the server-owned fields and history.
replaceDeck :: MonadIO m => Config -> Text -> DeckRef -> DeckWrite -> m (Either Failure Deck)
replaceDeck cfg token deckRef write = withDeckPath deckRef \path -> do
  req <- buildRequest cfg path
  send $ bearer token $ jsonBody "PUT" (toJSON write) req

{- | Create the next deck in a history chain and link it to this one.

The body is the child's desired content, so an upgrade that only grants
experience still sends the parent's cards. A parent that already has a child is
a 'conflict' -- and for an ArkhamDB parent, so is a child that was created
upstream but could not then be written, which leaves it linked there with its
id in the failure's description.
-}
upgradeDeck :: MonadIO m => Config -> Text -> DeckRef -> DeckWrite -> m (Either Failure Deck)
upgradeDeck cfg token deckRef write = withDeckPath deckRef \path -> do
  req <- buildRequest cfg $ path <> "/upgrade"
  send $ bearer token $ jsonBody "POST" (toJSON write) req

{- | Deck routes are addressed by provider and id. An id that is neither an
account string nor a positive ArkhamDB integer is not a request worth sending.
-}
withDeckPath
  :: Monad m => DeckRef -> (Text -> m (Either Failure a)) -> m (Either Failure a)
withDeckPath deckRef f = case deckPath deckRef of
  Nothing ->
    pure
      $ Left
      $ Failure 0 "invalid_request"
      $ "Not a deck arkham.build can address: "
      <> decodeUtf8 (BSL.toStrict $ encode deckRef.refId)
      <> " on "
      <> deckRef.refSource
  Just path -> f path

deckPath :: DeckRef -> Maybe Text
deckPath deckRef = do
  guard $ deckRef.refSource `elem` [accountSource, arkhamdbSource]
  segment <- idSegment deckRef.refId
  pure $ "/v2/user/decks/" <> deckRef.refSource <> "/" <> segment

-- | ArkhamDB ids are integers in JSON and decimal strings in a path.
idSegment :: Value -> Maybe Text
idSegment = \case
  String t -> do
    guard $ not (T.null t)
    pure $ decodeUtf8 $ urlEncode False $ encodeUtf8 t
  n@(Number _) -> do
    let t = decodeUtf8 $ BSL.toStrict $ encode n
    guard $ not (T.null t) && T.all isDigit t
    pure t
  _ -> Nothing

jsonBody :: ByteString -> Value -> Request -> Request
jsonBody method value req =
  req
    { HTTP.method = method
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
