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
  | BothTarget Target Target
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

class Targetable a where
  toTarget :: a -> Target
  isTarget :: a -> Target -> Bool
  isTarget = (==) . toTarget

instance Targetable Target where
  toTarget = id

instance Targetable ActId where
  toTarget = ActTarget

instance Targetable AgendaId where
  toTarget = AgendaTarget

instance Targetable CardId where
  toTarget = CardIdTarget

instance Targetable LocationId where
  toTarget = LocationTarget

instance Targetable EnemyId where
  toTarget = EnemyTarget

instance Targetable TreacheryId where
  toTarget = TreacheryTarget

instance Targetable InvestigatorId where
  toTarget = InvestigatorTarget

instance Targetable AssetId where
  toTarget = AssetTarget

instance Targetable EventId where
  toTarget = EventTarget

instance Targetable SkillId where
  toTarget = SkillTarget

toActionTarget :: Target -> Target
toActionTarget (ProxyTarget _ actionTarget) = actionTarget
toActionTarget target = target

toProxyTarget :: Target -> Target
toProxyTarget (ProxyTarget proxyTarget _) = proxyTarget
toProxyTarget target = target
