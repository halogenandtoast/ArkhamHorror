{-# LANGUAGE TemplateHaskell #-}

module Arkham.Target (
  module Arkham.Target,
) where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Ability.Types
import Arkham.Action
import {-# SOURCE #-} Arkham.Card
import {-# SOURCE #-} Arkham.Card.EncounterCard
import {-# SOURCE #-} Arkham.Card.PlayerCard
import Arkham.ChaosToken.Types
import Arkham.Id
import Arkham.Matcher.Agenda
import Arkham.Matcher.Enemy
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
  | SkillTestTarget SkillTestId
  | TreacheryTarget TreacheryId
  | EncounterDeckTarget
  | ScenarioDeckTarget
  | AgendaDeckTarget
  | ActDeckTarget
  | GameTarget
  | AgendaTarget AgendaId
  | ActTarget ActId
  | CardIdTarget CardId
  | CardCostTarget CardId
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
  | ThisTarget -- Used with withModifiers
  deriving stock (Show, Eq, Ord, Data, Generic)

instance HasField "asset" Target (Maybe AssetId) where
  getField = \case
    AssetTarget aid -> Just aid
    ProxyTarget (CardIdTarget _) t -> t.asset
    ProxyTarget t _ -> t.asset
    _ -> Nothing

instance HasField "enemy" Target (Maybe EnemyId) where
  getField = \case
    EnemyTarget aid -> Just aid
    ProxyTarget (CardIdTarget _) t -> t.enemy
    ProxyTarget t _ -> t.enemy
    _ -> Nothing

instance HasField "location" Target (Maybe LocationId) where
  getField = \case
    LocationTarget aid -> Just aid
    ProxyTarget (CardIdTarget _) t -> t.location
    ProxyTarget t _ -> t.location
    _ -> Nothing

instance HasField "investigator" Target (Maybe InvestigatorId) where
  getField = \case
    InvestigatorTarget aid -> Just aid
    InvestigatorHandTarget aid -> Just aid
    InvestigatorDiscardTarget aid -> Just aid
    ResourceTarget aid -> Just aid
    InvestigationTarget aid _ -> Just aid
    AbilityTarget aid _ -> Just aid
    ProxyTarget (CardIdTarget _) t -> t.investigator
    ProxyTarget t _ -> t.investigator
    _ -> Nothing

target_ :: Target -> Target
target_ = id
{-# INLINE target_ #-}

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

instance Targetable BatchId where
  toTarget = BatchTarget

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

instance Targetable SkillTestId where
  toTarget = SkillTestTarget

toActionTarget :: Target -> Target
toActionTarget (ProxyTarget _ actionTarget) = actionTarget
toActionTarget target = target

toProxyTarget :: Target -> Target
toProxyTarget (ProxyTarget proxyTarget _) = proxyTarget
toProxyTarget target = target

_EnemyTarget :: Traversal' Target EnemyId
_EnemyTarget f (EnemyTarget enemy) = EnemyTarget <$> f enemy
_EnemyTarget _ other = pure other

data ActionTarget
  = FirstOneOfPerformed [Action]
  | IsAction Action
  | EnemyAction Action EnemyMatcher
  | IsAnyAction
  | AnyActionTarget [ActionTarget]
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "parley" ActionTarget where
  fromLabel = IsAction #parley

instance IsLabel "play" ActionTarget where
  fromLabel = IsAction #play

instance IsLabel "engage" ActionTarget where
  fromLabel = IsAction #engage

instance IsLabel "resource" ActionTarget where
  fromLabel = IsAction #resource

instance IsLabel "draw" ActionTarget where
  fromLabel = IsAction #draw

instance IsLabel "move" ActionTarget where
  fromLabel = IsAction #move

instance IsLabel "evade" ActionTarget where
  fromLabel = IsAction #evade

instance IsLabel "investigate" ActionTarget where
  fromLabel = IsAction #investigate

instance IsLabel "resign" ActionTarget where
  fromLabel = IsAction #resign

mconcat
  [ deriveJSON defaultOptions ''ActionTarget
  , deriveToJSON defaultOptions ''Target
  ]

instance FromJSON Target where
  parseJSON = withObject "Target" \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "CardTarget" -> do
        card :: Card <- o .: "contents"
        pure $ CardIdTarget card.id
      _ -> $(mkParseJSON defaultOptions ''Target) (Object o)

instance FromJSONKey Target
instance ToJSONKey Target
