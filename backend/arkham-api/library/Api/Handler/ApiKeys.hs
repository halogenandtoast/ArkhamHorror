{- | Managing one's own API keys.

Every handler here is JWT-only -- it calls 'getRequestUserId' rather than
'getScopedCaller' -- so a key can never mint another key, widen its own scopes or
revoke a sibling. Whatever an agent holding a key can reach, it cannot reach the
means to reach more.
-}
module Api.Handler.ApiKeys (
  getApiV1ApiKeysR,
  getApiV1ApiKeySelfR,
  postApiV1ApiKeysR,
  deleteApiV1ApiKeyR,
) where

import Auth.ApiKey qualified as ApiKey
import Data.Aeson (withObject, (.!=), (.:?))
import Data.Text qualified as T
import Data.Time.Clock
import Database.Persist qualified as DB
import Import hiding ((==.))
import Import qualified as P
import Network.HTTP.Types (status404)

{- | What the owner is shown: everything except the digest.

The digest is not much of a secret -- it is a hash, useless to anyone holding it --
but there is no reason to serve it. The row's id is what the list is actually for,
since revoking is the only thing anyone does from there.
-}
apiKeyJson :: Entity ArkhamApiKey -> Value
apiKeyJson (Entity keyId key) =
  object
    [ "id" .= keyId
    , "name" .= arkhamApiKeyName key
    , "prefix" .= arkhamApiKeyPrefix key
    , "scopes" .= words (arkhamApiKeyScopes key)
    , "lastUsedAt" .= arkhamApiKeyLastUsedAt key
    , "expiresAt" .= arkhamApiKeyExpiresAt key
    , "revokedAt" .= arkhamApiKeyRevokedAt key
    , "createdAt" .= arkhamApiKeyCreatedAt key
    ]

-- | The user's keys, newest first.
getApiV1ApiKeysR :: Handler [Value]
getApiV1ApiKeysR = do
  userId <- getRequestUserId
  rows <- runDB $ P.selectList [ArkhamApiKeyUserId P.==. userId] [P.Desc ArkhamApiKeyCreatedAt]
  pure $ map apiKeyJson rows

{- | Who the caller is, and what their credential may do.

The one endpoint an API key may call about itself, and it needs no scope: a
credential is always allowed to ask what it is. Answering lets a client offer only
the tools the key can actually use, instead of listing writes that will 403 --
while leaving the decision itself with this server, which is the only thing that
should be making it.
-}
getApiV1ApiKeySelfR :: Handler Value
getApiV1ApiKeySelfR = do
  caller <- getScopedCaller []
  user <- runDB $ get404 (callerUserId caller)
  name <- case callerApiKeyId caller of
    Nothing -> pure Nothing
    Just keyId -> runDB $ fmap arkhamApiKeyName <$> DB.get keyId
  pure
    $ object
      [ "username" .= userUsername user
      , "scopes" .= callerScopes caller
      , "credential" .= maybe ("session" :: Text) (const "apiKey") (callerApiKeyId caller)
      , "keyName" .= name
      ]

data CreateApiKey = CreateApiKey
  { createApiKeyName :: Text
  , createApiKeyScopes :: [Text]
  , createApiKeyExpiresInDays :: Maybe Int
  }

instance FromJSON CreateApiKey where
  parseJSON = withObject "CreateApiKey" \o ->
    CreateApiKey
      <$> o
      .: "name"
      <*> o
      .:? "scopes"
      .!= [ApiKey.cardsRead]
      <*> o
      .:? "expiresInDays"

{- | Mint a key and show it once.

The plaintext is in this one response and nowhere else -- only its digest is
stored -- so the response says so rather than leaving the caller to find out by
coming back for it.

A key is capped at 25 per user: enough for a client per machine and then some,
and few enough that the table cannot be used as free storage.
-}
postApiV1ApiKeysR :: Handler Value
postApiV1ApiKeysR = do
  userId <- getRequestUserId
  CreateApiKey {..} <- requireCheckJsonBody
  let name = T.strip createApiKeyName
  when (T.null name) $ invalidArgs ["A key needs a name, so you can tell it from the others"]
  when (T.length name > 80) $ invalidArgs ["That name is too long"]

  let scopes = ApiKey.parseScopes (T.unwords createApiKeyScopes)
  when (null scopes)
    $ invalidArgs
      ["A key with no scopes could do nothing. Ask for: " <> ApiKey.renderScopes ApiKey.allScopes]

  existing <- runDB $ P.count [ArkhamApiKeyUserId P.==. userId, ArkhamApiKeyRevokedAt P.==. Nothing]
  when (existing >= 25) $ invalidArgs ["You already have 25 live keys; revoke one first"]

  now <- liftIO getCurrentTime
  minted <- ApiKey.mintKey
  let expiry = do
        days <- createApiKeyExpiresInDays
        guard (days > 0)
        pure $ addUTCTime (fromIntegral days * nominalDay) now

  void
    $ runDB
    $ P.insert
      ArkhamApiKey
        { arkhamApiKeyUserId = userId
        , arkhamApiKeyName = name
        , arkhamApiKeyPrefix = ApiKey.mintedPrefix minted
        , arkhamApiKeyDigest = ApiKey.mintedDigest minted
        , arkhamApiKeyScopes = ApiKey.renderScopes scopes
        , arkhamApiKeyLastUsedAt = Nothing
        , arkhamApiKeyExpiresAt = expiry
        , arkhamApiKeyRevokedAt = Nothing
        , arkhamApiKeyUsageWindowStart = Nothing
        , arkhamApiKeyUsageCount = 0
        , arkhamApiKeyCreatedAt = now
        }

  pure
    $ object
      [ "key" .= ApiKey.mintedPlaintext minted
      , "prefix" .= ApiKey.mintedPrefix minted
      , "name" .= name
      , "scopes" .= scopes
      , "expiresAt" .= expiry
      , "note"
          .= ( "This is the only time the key is shown -- only its digest is stored. "
                 <> "Send it as `Authorization: Bearer <key>`."
                 :: Text
             )
      ]

{- | Revoke a key.

Marked rather than deleted, so the row keeps saying what it was and when it was
last used. Revocation takes effect on the next request: 'callerFromKey' checks the
column every time rather than caching anything.
-}
deleteApiV1ApiKeyR :: ArkhamApiKeyId -> Handler ()
deleteApiV1ApiKeyR keyId = do
  userId <- getRequestUserId
  now <- liftIO getCurrentTime
  runDB do
    key <- DB.get keyId >>= maybe (lift $ sendResponseStatus status404 ("No such key" :: Text)) pure
    -- Not `permissionDenied`: whether this id exists is not the caller's business.
    when (arkhamApiKeyUserId key /= userId)
      $ lift
      $ sendResponseStatus status404 ("No such key" :: Text)
    P.update keyId [ArkhamApiKeyRevokedAt P.=. Just now]
