module Arkham.Types.Target
  ( module Arkham.Types.Target
  ) where

import Arkham.Prelude

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Id
import Arkham.Types.EffectId
import Arkham.Types.Id
import Arkham.Types.Phase
import Arkham.Types.Token
import Arkham.Types.Trait

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
  | YouTarget
  | InvestigationTarget InvestigatorId LocationId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
