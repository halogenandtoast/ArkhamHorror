{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.LogEntry (
  module Entity.Arkham.LogEntry,
) where

import Relude

import Data.Aeson.Types
import Data.Time.Clock
import Database.Persist.Postgresql.JSON ()
import Database.Persist.TH
import Entity.Arkham.Game
import Json
import Orphans ()

share
  [mkPersist sqlSettings]
  [persistLowerCase|
ArkhamLogEntry sql=arkham_log_entries
  body Text
  arkhamGameId ArkhamGameId OnDeleteCascade
  step Int
  createdAt UTCTime
  deriving Generic Show
|]

instance ToJSON ArkhamLogEntry where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamLogEntry"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamLogEntry"

instance FromJSON ArkhamLogEntry where
  parseJSON = genericParseJSON $ aesonOptions $ Just "arkhamLogEntry"
