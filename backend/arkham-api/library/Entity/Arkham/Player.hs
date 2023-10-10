{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.Player (
  module Entity.Arkham.Player,
) where

import Data.UUID (UUID)
import Database.Persist.TH
import Entity.Arkham.Game
import Entity.User
import Json
import Orphans ()
import Relude

mkPersist
  sqlSettings
  [persistLowerCase|
ArkhamPlayer sql=arkham_players
  Id UUID default=uuid_generate_v4()
  userId UserId OnDeleteCascade
  arkhamGameId ArkhamGameId OnDeleteCascade
  investigatorId Text
  UniquePlayer userId arkhamGameId
  deriving Generic Show
|]

instance ToJSON ArkhamPlayer where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamPlayer"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamPlayer"
