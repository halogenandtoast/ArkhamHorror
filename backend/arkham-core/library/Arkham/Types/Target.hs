module Arkham.Types.Target
  ( Target(..)
  , ForSkillTest(..)
  ) where

import ClassyPrelude

import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.Id
import Arkham.Types.EffectId
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import Data.Aeson

data ForSkillTest = ForSkillTest

data Target
  = AssetTarget AssetId
  | CampaignTarget CampaignId
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
  | AgendaTarget AgendaId
  | ActTarget ActId
  | CardIdTarget CardId
  | CardCodeTarget CardCode
  | SearchedCardTarget InvestigatorId CardId
  | EventTarget EventId
  | SkillTarget SkillId
  | SkillTestInitiatorTarget Target
  | TokenTarget TokenId
  | TokenFaceTarget Token
  | DrawnTokenTarget DrawnToken
  | TestTarget
  | EncounterCardTarget EncounterCard
  | ResourceTarget
  | InvestigationTarget InvestigatorId LocationId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)
