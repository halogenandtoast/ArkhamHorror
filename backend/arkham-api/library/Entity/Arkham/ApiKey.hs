{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.ApiKey (
  module Entity.Arkham.ApiKey,
) where

import Data.Time.Clock
import Data.UUID (UUID)
import Database.Persist.TH
import Entity
import Entity.User
import Orphans ()
import Relude

{- | A scoped, revocable credential for programmatic access.

The account token cannot do this job. 'Auth.JWT.jsonToToken' sets only @iss@ and
@iat@, so a login token never expires, authorises everything its owner can do --
decks, games, @DELETE \/account@ -- and cannot be revoked on its own: the only
lever is rotating the global signing secret, which signs out every user at once.
Handing one to an agent is handing over the account, permanently.

A key is none of those things. It carries only the scopes it was granted, it can
be revoked by itself, and it records when it was last used so its owner can see
whether it is still wanted.

Only 'arkhamApiKeyDigest' is stored, and the plaintext is shown once at creation
and never again.

The fields worth a word:

* @prefix@ -- the leading, non-secret part of the key. It is what the UI lists and
  what a log line can name, so a user with four keys can tell which one to revoke
  without anyone holding the whole thing again.
* @digest@ -- SHA-256 of the whole key. A key is high-entropy random bytes rather
  than a password, so one round is right: bcrypt exists to make guessing a
  low-entropy secret slow, there is nothing here to guess, and paying bcrypt on
  every API call would be a self-inflicted rate limit.
* @scopes@ -- space-separated, in the manner of an OAuth scope string, so this does
  not have to change if keys are later issued by an authorization server.
* @usageWindowStart@\/@usageCount@ -- a fixed-window rate limiter, kept on the row.
  In the app it could only be per-replica, and a limit a second replica doubles is
  not a limit; here it also holds for someone calling the API directly rather than
  through the MCP server.
-}
mkEntity
  $(discoverEntities)
  [persistLowerCase|
ArkhamApiKey sql=arkham_api_keys
  Id UUID default=uuid_generate_v4()
  userId UserId OnDeleteCascade
  name Text
  prefix Text
  digest Text
  scopes Text
  lastUsedAt UTCTime Maybe
  expiresAt UTCTime Maybe
  revokedAt UTCTime Maybe
  usageWindowStart UTCTime Maybe
  usageCount Int
  createdAt UTCTime
  UniqueApiKeyDigest digest
  deriving Generic Show
|]
