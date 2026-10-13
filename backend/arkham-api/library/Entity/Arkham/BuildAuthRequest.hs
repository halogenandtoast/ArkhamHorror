{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.BuildAuthRequest (
  module Entity.Arkham.BuildAuthRequest,
) where

import Data.Time.Clock
import Data.UUID (UUID)
import Database.Persist.TH
import Entity
import Entity.User
import Orphans ()
import Relude

{- | An arkham.build authorization in flight.

The row's id is the OAuth @state@. The callback arrives as a browser redirect
with no @Authorization@ header, so looking the state up here is the only thing
that says whose connection is being completed.
-}
mkEntity
  $(discoverEntities)
  [persistLowerCase|
ArkhamBuildAuthRequest sql=arkham_build_auth_requests
  Id UUID default=uuid_generate_v4()
  userId UserId OnDeleteCascade
  redirectUri Text
  scopes Text
  returnTo Text
  createdAt UTCTime
  deriving Generic Show
|]
