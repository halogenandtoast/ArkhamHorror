{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.GameRaw (
  module Entity.Arkham.GameRaw,
) where

import Relude

import Api.Arkham.Types.MultiplayerVariant
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
ArkhamGameRaw sql=arkham_games
  Id UUID default=uuid_generate_v4()
  name Text
  currentData Value
  step Int
  multiplayerVariant MultiplayerVariant
  createdAt UTCTime
  updatedAt UTCTime
  deriving Generic Show
|]

instance ToJSON ArkhamGameRaw where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamGameRaw"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamGameRaw"
