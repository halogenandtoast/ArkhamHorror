{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.Game (
  module Entity.Arkham.Game,
) where

import Relude

import Api.Arkham.Types.MultiplayerVariant
import Arkham.Game
import Data.Aeson.Types
import Data.Time.Clock
import Data.UUID
import Database.Persist.Postgresql.JSON ()
import Database.Persist.TH
import Json
import Orphans ()

share
  [mkPersist sqlSettings]
  [persistLowerCase|
ArkhamGame sql=arkham_games
  Id UUID default=uuid_generate_v4()
  name Text
  currentData Game
  step Int
  multiplayerVariant MultiplayerVariant
  createdAt UTCTime
  updatedAt UTCTime
  deriving Generic Show
|]

instance ToJSON ArkhamGame where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamGame"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamGame"
