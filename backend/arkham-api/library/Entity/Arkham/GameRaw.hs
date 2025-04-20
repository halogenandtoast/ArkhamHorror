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
import Database.Esqueleto.Experimental (ToBaseId (..))
import Database.Persist.Postgresql.JSON ()
import Database.Persist.TH
import Entity.Arkham.Game
import Json
import Orphans ()

mkPersist
  sqlSettings
  [persistLowerCase|
ArkhamGameRaw sql=arkham_games
  Id ArkhamGameId
  name Text
  currentData Value
  step Int
  multiplayerVariant MultiplayerVariant
  createdAt UTCTime
  updatedAt UTCTime
  deriving Generic Show
|]

instance ToBaseId ArkhamGameRaw where
  type BaseEnt ArkhamGameRaw = ArkhamGame
  toBaseIdWitness = ArkhamGameRawKey

instance ToJSON ArkhamGameRaw where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamGameRaw"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamGameRaw"
