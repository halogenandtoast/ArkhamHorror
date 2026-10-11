{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.BuildConnection (
  module Entity.Arkham.BuildConnection,
) where

import Data.Time.Clock
import Data.UUID (UUID)
import Database.Persist.TH
import Entity
import Entity.User
import Orphans ()
import Relude

{- | A user's link to their arkham.build account.

Both tokens are stored usable rather than digested, which is the whole point of
them; what keeps that honest is the scopes we ask for. @refreshToken@ rotates on
every use and works exactly once, so an exchange holds @FOR UPDATE@ on the row
until the replacement is written.
-}
mkEntity
  $(discoverEntities)
  [persistLowerCase|
ArkhamBuildConnection sql=arkham_build_connections
  Id UUID default=uuid_generate_v4()
  userId UserId OnDeleteCascade
  accountId Text
  username Text
  scopes Text
  accessToken Text
  accessTokenExpiresAt UTCTime
  refreshToken Text
  refreshTokenExpiresAt UTCTime
  createdAt UTCTime
  updatedAt UTCTime
  UniqueArkhamBuildConnectionUser userId
  deriving Generic Show
|]
