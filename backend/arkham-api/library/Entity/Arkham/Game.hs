{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.Game (module Entity.Arkham.Game) where

import Api.Arkham.Types.MultiplayerVariant
import Arkham.Game
import Data.Aeson.Types
import Data.Time.Clock
import Data.UUID
import Database.Persist.Postgresql.JSON ()
import Database.Persist.TH
import Entity
import GHC.Records
import Json
import Orphans ()
import Relude

mkEntity
  $(discoverEntities)
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

instance HasField "variant" ArkhamGame MultiplayerVariant where
  getField = arkhamGameMultiplayerVariant
