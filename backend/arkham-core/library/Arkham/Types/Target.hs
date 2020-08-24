module Arkham.Types.Target
  ( Target(..)
  )
where

import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.Card.Id
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Aeson

data Target
  = AssetTarget AssetId
  | EnemyTarget EnemyId
  | InvestigatorTarget InvestigatorId
  | LocationTarget LocationId
  | SkillTestTarget
  | TreacheryTarget TreacheryId
  | EncounterDeckTarget
  | AgendaTarget AgendaId
  | ActTarget ActId
  | CardIdTarget CardId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
