module Arkham.Types.Target
  ( Target(..)
  )
where

import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.Card.Id
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.SkillId
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Aeson

data Target
  = AssetTarget AssetId
  | EnemyTarget EnemyId
  | InvestigatorTarget InvestigatorId
  | LocationTarget LocationId
  | SkillTestTarget
  | AfterSkillTestTarget
  | TreacheryTarget TreacheryId
  | EncounterDeckTarget
  | AgendaTarget AgendaId
  | ActTarget ActId
  | CardIdTarget CardId
  | EventTarget EventId
  | SkillTarget SkillId
  | EnemyJustEvadedTarget
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
