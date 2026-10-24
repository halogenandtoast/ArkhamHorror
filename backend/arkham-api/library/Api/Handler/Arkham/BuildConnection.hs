{- | Connecting a user's account to their arkham.build account over OAuth 2.0.

We are a confidential client, so the whole exchange happens here: the browser
carries only the authorization request and the callback, and never holds a token
or the client secret.

The callback arrives as a plain browser redirect with no @Authorization@ header,
so it cannot know who it belongs to on its own. The @state@ parameter is the row
id of an 'ArkhamBuildAuthRequest' written by the authenticated request that
started the authorization; looking it up is both the CSRF check the spec asks for
and the only thing that identifies the user.
-}
module Api.Handler.Arkham.BuildConnection (
  getApiV1ArkhamBuildConnectionR,
  deleteApiV1ArkhamBuildConnectionR,
  postApiV1ArkhamBuildAuthorizeR,
  postApiV1ArkhamBuildVerifyR,
  getApiV1ArkhamBuildDecksR,
  getApiAuthArkhamBuildCallbackR,
  arkhamBuildAccessToken,
  disconnectArkhamBuild,
) where

import Api.Handler.Arkham.CustomCards (registerUserCustomCards)
import Api.Handler.Arkham.Decks (normalizeArkhamBuildDeckCodes)
import Arkham.Card.CardCode (CardCode (..))
import Arkham.Decklist.Type (ArkhamDBDecklist (..))
import Arkham.Id (InvestigatorId (..))
import Arkham.Investigator.Cards (allInvestigatorCards)
import Arkham.Name (toTitle)
import ArkhamBuild.OAuth qualified as AB
import Data.Aeson (Result (..), fromJSON, withObject, (.:?))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock
import Data.Traversable (for)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Database.Esqueleto.Experimental qualified as E
import Database.Persist qualified as DB
import Import
import Network.HTTP.Types.Status qualified as Status

{- | A route in the frontend, which uses hash history -- so the outcome has to
ride inside the fragment. Sent as a path the browser resolves against this
origin, which is the SPA's own: the callback is only ever reached through it.
-}
settingsPath :: Text
settingsPath = "/settings"

returnUrl :: Text -> Text -> Text
returnUrl path outcome = "/#" <> path <> "?arkhamBuild=" <> outcomeSlug outcome

{- | How long a started authorization is good for; arkham.build expires its own
request after the same 15 minutes.
-}
authorizationWindow :: NominalDiffTime
authorizationWindow = 15 * 60

{- | Refresh an access token this close to its hour rather than letting a call
fail on one that expires mid-flight.
-}
refreshMargin :: NominalDiffTime
refreshMargin = 5 * 60

arkhamBuildConfig :: Handler (Maybe AB.Config)
arkhamBuildConfig = do
  settings <- getsYesod appSettings
  pure do
    clientId <- appArkhamBuildClientId settings
    clientSecret <- appArkhamBuildClientSecret settings
    pure
      AB.Config
        { AB.apiBase = appArkhamBuildApiBase settings
        , AB.clientId = clientId
        , AB.clientSecret = clientSecret
        , AB.redirectUri = appArkhamBuildRedirectUri settings
        }

requireArkhamBuildConfig :: Handler AB.Config
requireArkhamBuildConfig =
  arkhamBuildConfig >>= \case
    Just cfg -> pure cfg
    Nothing ->
      sendStatusJSON
        Status.status503
        $ object
          [ "error" .= ("not_configured" :: Text)
          , "message" .= ("This server has no arkham.build client credentials." :: Text)
          ]

{- | @missingScopes@ is what this grant was never given. A connection made before
'AB.requestedScopes' grew is still valid for what it holds, so the answer is for
the user to reconnect rather than for us to treat it as broken.
-}
connectionJson :: ArkhamBuildConnection -> Value
connectionJson c =
  object
    [ "accountId" .= arkhamBuildConnectionAccountId c
    , "username" .= arkhamBuildConnectionUsername c
    , "scopes" .= granted
    , "missingScopes" .= filter (`notElem` granted) AB.requestedScopes
    , "connectedAt" .= arkhamBuildConnectionCreatedAt c
    , "refreshedAt" .= arkhamBuildConnectionUpdatedAt c
    , "expiresAt" .= arkhamBuildConnectionRefreshTokenExpiresAt c
    ]
 where
  granted = words (arkhamBuildConnectionScopes c)

failureJson :: AB.Failure -> Value
failureJson f = object ["error" .= AB.code f, "message" .= AB.description f]

{- | A failure the user can act on (reconnect) is theirs; anything else is
arkham.build's, and a 502 says so rather than blaming the request.
-}
sendFailure :: AB.Failure -> Handler a
sendFailure f
  | AB.status f `elem` [401, 403, 404] = sendStatusJSON Status.status400 (failureJson f)
  | otherwise = sendStatusJSON Status.status502 (failureJson f)

{- | Whether this server can connect at all, and whether this user has.

No token is served, ever -- the browser has no use for one it cannot send
without exposing it.
-}
getApiV1ArkhamBuildConnectionR :: Handler Value
getApiV1ArkhamBuildConnectionR = do
  userId <- getRequestUserId
  mCfg <- arkhamBuildConfig
  mConn <- runDB $ getBy $ UniqueArkhamBuildConnectionUser userId
  pure
    $ object
      [ "configured" .= isJust mCfg
      , "scopes" .= AB.requestedScopes
      , "connection" .= fmap (connectionJson . entityVal) mConn
      ]

newtype StartAuthorization = StartAuthorization {startReturnTo :: Maybe Text}

instance FromJSON StartAuthorization where
  parseJSON = withObject "StartAuthorization" \o -> StartAuthorization <$> o .:? "returnTo"

{- | Start an authorization and hand back the URL to send the browser to.

The frontend navigates rather than opening a window, so @returnTo@ is how it
says which page to come back to. Only a path on this site is accepted, so a
callback cannot be talked into bouncing the browser off-site.
-}
postApiV1ArkhamBuildAuthorizeR :: Handler Value
postApiV1ArkhamBuildAuthorizeR = do
  userId <- getRequestUserId
  cfg <- requireArkhamBuildConfig
  StartAuthorization {..} <- requireCheckJsonBody
  now <- liftIO getCurrentTime
  authState <- liftIO UUID.nextRandom

  runDB do
    deleteWhere [ArkhamBuildAuthRequestUserId ==. userId]
    deleteWhere
      [ArkhamBuildAuthRequestCreatedAt <. addUTCTime (negate authorizationWindow) now]
    insertKey (ArkhamBuildAuthRequestKey authState)
      $ ArkhamBuildAuthRequest
        { arkhamBuildAuthRequestUserId = userId
        , arkhamBuildAuthRequestRedirectUri = AB.redirectUri cfg
        , arkhamBuildAuthRequestScopes = T.unwords AB.requestedScopes
        , arkhamBuildAuthRequestReturnTo = fromMaybe settingsPath (safeReturnTo =<< startReturnTo)
        , arkhamBuildAuthRequestCreatedAt = now
        }

  pure $ object ["url" .= AB.authorizeUrl cfg AB.requestedScopes (UUID.toText authState)]

safeReturnTo :: Text -> Maybe Text
safeReturnTo path = do
  guard $ "/" `T.isPrefixOf` path
  guard $ not $ "//" `T.isPrefixOf` path
  guard $ not $ T.any (`elem` ['\r', '\n', '\\']) path
  pure $ T.takeWhile (/= '?') path

{- | Where arkham.build sends the browser back to.

Lives at @\/api\/auth\/...@ rather than under @\/v1@ because arkham.build compares
the redirect URI as an exact string against the one registered for this client,
and that is the path that was registered.

Unauthenticated by necessity. Everything it is allowed to do comes from the
'ArkhamBuildAuthRequest' the @state@ names, and that row is consumed here whether
or not the rest of the exchange works -- a code is single use, so a replayed
callback must not get a second attempt at one.
-}
getApiAuthArkhamBuildCallbackR :: Handler Html
getApiAuthArkhamBuildCallbackR = do
  mState <- lookupGetParam "state"
  mCode <- lookupGetParam "code"
  mError <- lookupGetParam "error"
  now <- liftIO getCurrentTime

  mRequest <- case UUID.fromText =<< mState of
    Nothing -> pure Nothing
    Just authState -> runDB do
      let key = ArkhamBuildAuthRequestKey authState
      mRow <- DB.get key
      for_ mRow $ const $ DB.delete key
      pure mRow

  case mRequest of
    Nothing -> done settingsPath "invalid_state"
    Just request -> do
      let returnTo = arkhamBuildAuthRequestReturnTo request
      if diffUTCTime now (arkhamBuildAuthRequestCreatedAt request) > authorizationWindow
        then done returnTo "expired"
        else case (mError, mCode) of
          (Just err, _) -> done returnTo err
          (Nothing, Nothing) -> done returnTo "no_code"
          (Nothing, Just authCode) ->
            arkhamBuildConfig >>= \case
              Nothing -> done returnTo "not_configured"
              Just cfg ->
                AB.exchangeCode cfg authCode >>= \case
                  Left failure -> done returnTo (AB.code failure)
                  Right tokens ->
                    AB.fetchProfile cfg (AB.accessToken tokens) >>= \case
                      Left failure -> done returnTo (AB.code failure)
                      Right profile -> do
                        storeConnection (arkhamBuildAuthRequestUserId request) now tokens profile
                        done returnTo "connected"
 where
  done path outcome = redirect $ returnUrl path outcome

{- | `error` arrives from arkham.build, so it is reduced to the shape of an
OAuth error code before it goes back out in a URL.
-}
outcomeSlug :: Text -> Text
outcomeSlug =
  orUnknown . T.take 40 . T.filter (\c -> isAsciiLower c || isAsciiUpper c || c == '_')
 where
  orUnknown t = if T.null t then "unknown_error" else t

storeConnection :: UserId -> UTCTime -> AB.Tokens -> AB.Profile -> Handler ()
storeConnection userId now tokens profile = runDB do
  mExisting <- getBy $ UniqueArkhamBuildConnectionUser userId
  case mExisting of
    Just (Entity connectionId _) ->
      update
        connectionId
        [ ArkhamBuildConnectionAccountId =. AB.accountId profile
        , ArkhamBuildConnectionUsername =. AB.username profile
        , ArkhamBuildConnectionScopes =. AB.scope tokens
        , ArkhamBuildConnectionAccessToken =. AB.accessToken tokens
        , ArkhamBuildConnectionAccessTokenExpiresAt =. accessExpiry
        , ArkhamBuildConnectionRefreshToken =. AB.refreshToken tokens
        , ArkhamBuildConnectionRefreshTokenExpiresAt =. refreshExpiry
        , ArkhamBuildConnectionUpdatedAt =. now
        ]
    Nothing ->
      void
        $ insert
          ArkhamBuildConnection
            { arkhamBuildConnectionUserId = userId
            , arkhamBuildConnectionAccountId = AB.accountId profile
            , arkhamBuildConnectionUsername = AB.username profile
            , arkhamBuildConnectionScopes = AB.scope tokens
            , arkhamBuildConnectionAccessToken = AB.accessToken tokens
            , arkhamBuildConnectionAccessTokenExpiresAt = accessExpiry
            , arkhamBuildConnectionRefreshToken = AB.refreshToken tokens
            , arkhamBuildConnectionRefreshTokenExpiresAt = refreshExpiry
            , arkhamBuildConnectionCreatedAt = now
            , arkhamBuildConnectionUpdatedAt = now
            }
 where
  accessExpiry = addUTCTime (fromIntegral $ AB.expiresIn tokens) now
  refreshExpiry = addUTCTime AB.refreshLifetime now

{- | Prove the stored credentials still work, and re-read the account they belong
to while we are there.

The connection is otherwise only as true as it was the day it was made: a grant
revoked from arkham.build's own settings leaves our row looking healthy until
something tries to use it.
-}
postApiV1ArkhamBuildVerifyR :: Handler Value
postApiV1ArkhamBuildVerifyR = do
  userId <- getRequestUserId
  cfg <- requireArkhamBuildConfig
  arkhamBuildAccessToken cfg userId >>= \case
    Left failure -> sendFailure failure
    Right token ->
      AB.fetchProfile cfg token >>= \case
        Left failure -> sendFailure failure
        Right profile -> do
          now <- liftIO getCurrentTime
          runDB
            $ updateWhere
              [ArkhamBuildConnectionUserId ==. userId]
              [ ArkhamBuildConnectionAccountId =. AB.accountId profile
              , ArkhamBuildConnectionUsername =. AB.username profile
              , ArkhamBuildConnectionUpdatedAt =. now
              ]
          mConn <- runDB $ getBy $ UniqueArkhamBuildConnectionUser userId
          pure $ object ["connection" .= fmap (connectionJson . entityVal) mConn]

{- | The decks this user keeps on arkham.build, ready to be loaded.

Two calls upstream: a manifest naming every deck, then batches reading them. The
manifest's @arkhamdbSyncToken@ pins the ArkhamDB snapshot the batches read, so
every batch belonging to one manifest sends the same token.

A deck that does not decode as a decklist is dropped rather than failing the
request: one unreadable deck should not cost the player the list.
-}
getApiV1ArkhamBuildDecksR :: Handler Value
getApiV1ArkhamBuildDecksR = do
  userId <- getRequestUserId
  cfg <- requireArkhamBuildConfig
  -- A deck may name custom cards, which only resolve once this user's library is
  -- registered.
  registerUserCustomCards userId
  token <- arkhamBuildAccessToken cfg userId >>= either sendFailure pure

  manifest <- AB.fetchDeckManifest cfg token Nothing >>= either sendFailure pure

  -- ArkhamDB is a separate upstream that can be down on its own. When it is, the
  -- manifest still lists account decks and the sync token is absent; asking for
  -- an ArkhamDB deck in that state is a 503 for the whole batch.
  let wanted =
        [ entry
        | entry <- AB.entries manifest
        , AB.entrySource entry /= "arkhamdb" || usableArkhamdb manifest
        ]

  fetched <-
    fold <$> for (chunksOf AB.batchLimit wanted) \batch -> do
      raw <- AB.fetchDeckBatch cfg token (AB.syncToken manifest) batch >>= either sendFailure pure
      pure $ zip batch raw

  pure
    $ object
      [ "decks" .= mapMaybe (uncurry deckJson) (chainTips fetched)
      , "providers"
          .= object
            [ "account" .= AB.accountAvailable manifest
            , "arkhamdb" .= AB.arkhamdbAvailable manifest
            ]
      ]
 where
  usableArkhamdb manifest = AB.arkhamdbAvailable manifest && isJust (AB.syncToken manifest)

{- | Drop every deck that a later version of itself supersedes.

An upgrade is a new deck linked to its parent, so a deck upgraded eleven times is
eleven decks on the account, all with the same name. Only the tip of each chain is
worth offering; the rest are its history. A deck is a tip when nothing else in
the list names it as @previous_deck@ -- read that way round rather than from its
own @next_deck@ so a chain whose tip is somehow absent still offers something.
-}
chainTips :: [(AB.ManifestEntry, Value)] -> [(AB.ManifestEntry, Value)]
chainTips fetched = filter (not . superseded) fetched
 where
  -- A list rather than a set: a collection is hundreds of decks, not millions.
  parents = mapMaybe (previousDeck . snd) fetched
  superseded (entry, _) = AB.entryId entry `elem` parents
  previousDeck raw = case raw of
    Object o -> case KeyMap.lookup "previous_deck" o of
      Just Null -> Nothing
      other -> other
    _ -> Nothing

deckJson :: AB.ManifestEntry -> Value -> Maybe Value
deckJson entry raw = case fromJSON (normalizeArkhamBuildDeckCodes raw) of
  Error _ -> Nothing
  Success (decklist :: ArkhamDBDecklist) ->
    Just
      $ object
        [ "source" .= AB.entrySource entry
        , "id" .= AB.entryId entry
        , "updatedAt" .= AB.entryUpdatedAt entry
        , "decklist" .= withInvestigatorName decklist
        ]

{- | arkham.build leaves @investigator_name@ empty on an account deck, so the
list would show a portrait and no name. The decklist parser only fills it in when
the key is absent, and an empty string is not absent.
-}
withInvestigatorName :: ArkhamDBDecklist -> ArkhamDBDecklist
withInvestigatorName decklist
  | not (T.null decklist.investigator_name) = decklist
  | otherwise = decklist {investigator_name = maybe "" toTitle found}
 where
  code = unCardCode $ unInvestigatorId decklist.investigator_code
  found =
    asum
      [ Map.lookup (CardCode candidate) allInvestigatorCards
      | candidate <- [code, fromMaybe code (T.stripPrefix "c" code)]
      ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n <= 0 || null xs = []
  | otherwise = let (chunk, rest) = splitAt n xs in chunk : chunksOf n rest

{- | Hand back a usable access token, refreshing it first if it is nearly out.

A refresh token rotates and works exactly once, so the row is held @FOR UPDATE@
across the exchange: two requests refreshing at the same time would otherwise
each spend a token and one of them would store a replacement that the other had
already invalidated, leaving a connection that cannot be refreshed at all.
-}
arkhamBuildAccessToken :: AB.Config -> UserId -> Handler (Either AB.Failure Text)
arkhamBuildAccessToken cfg userId = runDB do
  rows <- E.select do
    connections <- E.from $ E.table @ArkhamBuildConnection
    E.where_ $ connections.userId E.==. E.val userId
    E.locking E.forUpdate
    pure connections
  case rows of
    [] -> pure $ Left $ AB.Failure 404 "not_connected" "This account is not connected to arkham.build."
    (Entity connectionId connection : _) -> do
      now <- liftIO getCurrentTime
      if addUTCTime refreshMargin now < arkhamBuildConnectionAccessTokenExpiresAt connection
        then pure $ Right $ arkhamBuildConnectionAccessToken connection
        else
          if arkhamBuildConnectionRefreshTokenExpiresAt connection <= now
            then
              pure
                $ Left
                $ AB.Failure 401 "authorization_expired" "Reconnect to arkham.build to keep using it."
            else
              AB.refreshTokens cfg (arkhamBuildConnectionRefreshToken connection) >>= \case
                Left failure -> pure $ Left failure
                Right tokens -> do
                  update
                    connectionId
                    [ ArkhamBuildConnectionScopes =. AB.scope tokens
                    , ArkhamBuildConnectionAccessToken =. AB.accessToken tokens
                    , ArkhamBuildConnectionAccessTokenExpiresAt
                        =. addUTCTime (fromIntegral $ AB.expiresIn tokens) now
                    , ArkhamBuildConnectionRefreshToken =. AB.refreshToken tokens
                    , ArkhamBuildConnectionRefreshTokenExpiresAt
                        =. addUTCTime AB.refreshLifetime now
                    , ArkhamBuildConnectionUpdatedAt =. now
                    ]
                  pure $ Right $ AB.accessToken tokens

{- | Hand the grant back.

The refresh token is revoked at arkham.build first, which takes its access tokens
with it, and the row goes whether or not that call worked -- the user asked to be
disconnected, and a grant they can also revoke from their own arkham.build
settings is not a reason to refuse.
-}
deleteApiV1ArkhamBuildConnectionR :: Handler ()
deleteApiV1ArkhamBuildConnectionR = getRequestUserId >>= disconnectArkhamBuild

-- | Also called when an account is deleted, so a grant is not left live upstream.
disconnectArkhamBuild :: UserId -> Handler ()
disconnectArkhamBuild userId = do
  mCfg <- arkhamBuildConfig
  mConn <- runDB $ getBy $ UniqueArkhamBuildConnectionUser userId
  for_ mConn \(Entity connectionId connection) -> do
    for_ mCfg \cfg ->
      AB.revokeRefreshToken cfg (arkhamBuildConnectionRefreshToken connection)
    runDB $ DB.delete connectionId
