module Arkham.Target (
  module Arkham.Target,
) where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Ability.Types
import {-# SOURCE #-} Arkham.Card
import Arkham.Card.CardCode
import Arkham.Card.Id
import Arkham.Id
import Arkham.Matcher
import Arkham.Phase
import Arkham.Token
import Arkham.Trait

data ForSkillTest = ForSkillTest

data Target
  = AssetTarget AssetId
  | EnemyTarget EnemyId
  | ScenarioTarget
  | EffectTarget EffectId
  | InvestigatorTarget InvestigatorId
  | InvestigatorHandTarget InvestigatorId -- used for cards in hand
  | InvestigatorDiscardTarget InvestigatorId -- used for cards in discard
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
  | CardTarget Card
  | CardCodeTarget CardCode
  | SearchedCardTarget CardId
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
  | ProxyTarget Target Target
  | StoryTarget CardCode
  | AgendaMatcherTarget AgendaMatcher
  | CampaignTarget
  | AbilityTarget InvestigatorId Ability
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

class IdToTarget a where
  idToTarget :: a -> Target

instance IdToTarget ActId where
  idToTarget = ActTarget

instance IdToTarget AgendaId where
  idToTarget = AgendaTarget

instance IdToTarget CardId where
  idToTarget = CardIdTarget

instance IdToTarget LocationId where
  idToTarget = LocationTarget

instance IdToTarget EnemyId where
  idToTarget = EnemyTarget

instance IdToTarget TreacheryId where
  idToTarget = TreacheryTarget

instance IdToTarget InvestigatorId where
  idToTarget = InvestigatorTarget

instance IdToTarget AssetId where
  idToTarget = AssetTarget

instance IdToTarget EventId where
  idToTarget = EventTarget

instance IdToTarget SkillId where
  idToTarget = SkillTarget

toActionTarget :: Target -> Target
toActionTarget (ProxyTarget _ actionTarget) = actionTarget
toActionTarget target = target

toProxyTarget :: Target -> Target
toProxyTarget (ProxyTarget proxyTarget _) = proxyTarget
toProxyTarget target = target
