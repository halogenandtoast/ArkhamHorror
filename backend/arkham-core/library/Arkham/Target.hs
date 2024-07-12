{-# LANGUAGE TemplateHaskell #-}

module Arkham.Target (
  module Arkham.Target,
) where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Ability.Types
import {-# SOURCE #-} Arkham.Card
import {-# SOURCE #-} Arkham.Card.EncounterCard
import {-# SOURCE #-} Arkham.Card.PlayerCard
import Arkham.ChaosToken
import Arkham.Id
import Arkham.Matcher
import Arkham.Phase
import Arkham.Tarot
import Arkham.Trait
import Control.Lens (Getting, Prism', prism')
import Data.Aeson.TH
import Data.Monoid (First)
import GHC.OverloadedLabels
import GHC.Records

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
  | PhaseTarget Phase
  | ChaosTokenTarget ChaosToken
  | ChaosTokenFaceTarget ChaosTokenFace
  | TestTarget
  | ResourceTarget InvestigatorId
  | YouTarget
  | InvestigationTarget InvestigatorId LocationId
  | ProxyTarget Target Target
  | StoryTarget StoryId
  | AgendaMatcherTarget AgendaMatcher
  | CampaignTarget
  | AbilityTarget InvestigatorId Ability
  | BothTarget Target Target
  | TarotTarget TarotCard
  | BatchTarget BatchId
  | ActiveCostTarget ActiveCostId
  | LabeledTarget Text Target -- Use with caution, this is not a real target
  deriving stock (Show, Eq, Ord, Data)

instance HasField "enemy" Target (Maybe EnemyId) where
  getField = \case
    EnemyTarget aid -> Just aid
    ProxyTarget (CardIdTarget _) t -> t.enemy
    ProxyTarget t _ -> t.enemy
    _ -> Nothing

bothTarget :: (Targetable a, Targetable b) => a -> b -> Target
bothTarget a b = BothTarget (toTarget a) (toTarget b)

actualTarget :: Target -> Target
actualTarget = \case
  LabeledTarget _ t -> actualTarget t
  ProxyTarget t _ -> actualTarget t
  SkillTestInitiatorTarget t -> actualTarget t
  BothTarget t1 t2 -> BothTarget (actualTarget t1) (actualTarget t2)
  other -> other

investigatorTarget :: Target -> Maybe InvestigatorId
investigatorTarget (InvestigatorTarget iid) = Just iid
investigatorTarget _ = Nothing

_InvestigatorTarget :: Prism' Target InvestigatorId
_InvestigatorTarget = prism' InvestigatorTarget investigatorTarget

instance IsLabel "encounterDeck" Target where
  fromLabel = EncounterDeckTarget

instance IsLabel "investigator" (Getting (First InvestigatorId) Target InvestigatorId) where
  fromLabel = _InvestigatorTarget

pattern Initiator :: Target -> Target
pattern Initiator t <- SkillTestInitiatorTarget t
  where
    Initiator t = SkillTestInitiatorTarget t

pattern InitiatorProxy :: Target -> Target -> Target
pattern InitiatorProxy t a <- SkillTestInitiatorTarget (ProxyTarget t a)
  where
    InitiatorProxy t a = SkillTestInitiatorTarget (ProxyTarget t a)

class WithTarget a where
  getTarget :: a -> Maybe Target
  setTarget :: Targetable target => target -> a -> a

maybeIsTarget :: (Targetable target, WithTarget a) => target -> a -> Bool
maybeIsTarget target a = maybe False (isTarget target) (getTarget a)

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

instance Targetable Card where
  toTarget = CardIdTarget . toCardId

instance Targetable PlayerCard where
  toTarget = CardIdTarget . toCardId

instance Targetable EncounterCard where
  toTarget = CardIdTarget . toCardId

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

instance Targetable StoryId where
  toTarget = StoryTarget

toActionTarget :: Target -> Target
toActionTarget (ProxyTarget _ actionTarget) = actionTarget
toActionTarget target = target

toProxyTarget :: Target -> Target
toProxyTarget (ProxyTarget proxyTarget _) = proxyTarget
toProxyTarget target = target

_EnemyTarget :: Traversal' Target EnemyId
_EnemyTarget f (EnemyTarget enemy) = EnemyTarget <$> f enemy
_EnemyTarget _ other = pure other

$(deriveJSON defaultOptions ''Target)

instance FromJSONKey Target
instance ToJSONKey Target
