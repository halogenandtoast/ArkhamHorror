module Arkham.Types.Target
  ( module Arkham.Types.Target
  ) where

import Arkham.Prelude

import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Id
import Arkham.Types.EffectId
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Phase
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.TreacheryId

data ForSkillTest = ForSkillTest

data Target
  = AssetTarget AssetId
  | EnemyTarget EnemyId
  | ScenarioTarget ScenarioId
  | EffectTarget EffectId
  | InvestigatorTarget InvestigatorId
  | LocationTarget LocationId
  | SetAsideLocationsTarget [Trait]
  | SkillTestTarget
  | AfterSkillTestTarget
  | TreacheryTarget TreacheryId
  | EncounterDeckTarget
  | ScenarioDeckTarget
  | AgendaDeckTarget
  | ActDeckTarget
  | AgendaTarget AgendaId
  | ActTarget ActId
  | CardIdTarget CardId
  | CardCodeTarget CardCode
  | SearchedCardTarget InvestigatorId CardId
  | EventTarget EventId
  | SkillTarget SkillId
  | SkillTestInitiatorTarget Target
  | TokenTarget Token
  | PhaseTarget Phase
  | TokenFaceTarget TokenFace
  | TestTarget
  | ResourceTarget
  | InvestigationTarget InvestigatorId LocationId
  | YouTarget
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
