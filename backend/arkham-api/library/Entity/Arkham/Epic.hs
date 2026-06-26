{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Persistence for Epic Multiplayer.
--
-- An 'ArkhamEpicEvent' is the aggregate that owns N otherwise-independent group
-- games and the small shared state across them. Group games are referenced by
-- FK through 'ArkhamEpicGroup' (so the central @arkham_games@ table is left
-- untouched). 'ArkhamEpicMember' records who is organizer / player. Each
-- shared-state mutation is recorded as an 'ArkhamEpicStep' carrying the
-- invertible 'SharedDelta' (for cross-group undo).
module Entity.Arkham.Epic (
  module Entity.Arkham.Epic,
) where

import Arkham.Epic.Types
import Data.Time.Clock
import Data.UUID (UUID)
import Database.Persist.Postgresql.JSON ()
import Database.Persist.TH
import Entity
import Entity.Arkham.Game
import Entity.User
import Json
import Orphans ()
import Relude

mkEntity
  $(discoverEntities)
  [persistLowerCase|
ArkhamEpicEvent sql=arkham_epic_events
  Id UUID default=uuid_generate_v4()
  name Text
  organizerUserId UserId OnDeleteCascade
  scenarioId Text Maybe
  campaignId Text Maybe
  difficulty Text
  sharedState SharedEventState
  totalInvestigators Int
  step Int
  createdAt UTCTime
  updatedAt UTCTime
  deriving Generic Show

ArkhamEpicGroup sql=arkham_epic_groups
  Id UUID default=uuid_generate_v4()
  arkhamEpicEventId ArkhamEpicEventId OnDeleteCascade
  ordinal Int
  arkhamGameId ArkhamGameId Maybe OnDeleteCascade
  name Text
  seatCount Int
  UniqueEpicGroupOrdinal arkhamEpicEventId ordinal
  deriving Generic Show

ArkhamEpicMember sql=arkham_epic_members
  Id UUID default=uuid_generate_v4()
  arkhamEpicEventId ArkhamEpicEventId OnDeleteCascade
  userId UserId OnDeleteCascade
  role EpicRole
  groupOrdinal Int Maybe
  UniqueEpicMember arkhamEpicEventId userId role
  deriving Generic Show

ArkhamEpicStep sql=arkham_epic_steps
  Id UUID default=uuid_generate_v4()
  arkhamEpicEventId ArkhamEpicEventId OnDeleteCascade
  step Int
  arkhamGameId ArkhamGameId Maybe
  gameStep Int Maybe
  delta SharedDelta
  createdAt UTCTime
  deriving Generic Show
|]

instance ToJSON ArkhamEpicEvent where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamEpicEvent"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamEpicEvent"

instance ToJSON ArkhamEpicGroup where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamEpicGroup"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamEpicGroup"

instance ToJSON ArkhamEpicMember where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamEpicMember"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamEpicMember"

instance ToJSON ArkhamEpicStep where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamEpicStep"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamEpicStep"
