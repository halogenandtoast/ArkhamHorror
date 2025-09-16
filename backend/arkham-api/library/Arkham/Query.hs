module Arkham.Query where

import {-# SOURCE #-} Arkham.Ability.Types
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import {-# SOURCE #-} Arkham.Source
import {-# SOURCE #-} Arkham.Target

type family QueryElement a where
  QueryElement AssetMatcher = AssetId
  QueryElement InvestigatorMatcher = InvestigatorId
  QueryElement PreyMatcher = InvestigatorId
  QueryElement LocationMatcher = LocationId
  QueryElement EnemyMatcher = EnemyId
  QueryElement TreacheryMatcher = TreacheryId
  QueryElement ExtendedCardMatcher = Card
  QueryElement DiscardedPlayerCardMatcher = PlayerCard
  QueryElement AbilityMatcher = Ability
  QueryElement SkillMatcher = SkillId
  QueryElement StoryMatcher = StoryId
  QueryElement EventMatcher = EventId
  QueryElement EffectMatcher = EffectId
  QueryElement ActMatcher = ActId
  QueryElement AgendaMatcher = AgendaId
  QueryElement ScenarioMatcher = ScenarioId
  QueryElement CampaignMatcher = CampaignId
  QueryElement RemainingActMatcher = CardCode
  QueryElement CardMatcher = Card
  QueryElement ChaosTokenMatcher = ChaosToken
  QueryElement TargetMatcher = Target
  QueryElement SourceMatcher = Source

data SomeQuery a where
  AssetQuery :: AssetMatcher -> SomeQuery (QueryElement AssetMatcher)
  InvestigatorQuery :: InvestigatorMatcher -> SomeQuery (QueryElement InvestigatorMatcher)
  PreyQuery :: PreyMatcher -> SomeQuery (QueryElement PreyMatcher)
  LocationQuery :: LocationMatcher -> SomeQuery (QueryElement LocationMatcher)
  EnemyQuery :: EnemyMatcher -> SomeQuery (QueryElement EnemyMatcher)
  TreacheryQuery :: TreacheryMatcher -> SomeQuery (QueryElement TreacheryMatcher)
  ExtendedCardQuery :: ExtendedCardMatcher -> SomeQuery (QueryElement ExtendedCardMatcher)
  DiscardedPlayerCardQuery
    :: DiscardedPlayerCardMatcher -> SomeQuery (QueryElement DiscardedPlayerCardMatcher)
  AbilityQuery :: AbilityMatcher -> SomeQuery (QueryElement AbilityMatcher)
  SkillQuery :: SkillMatcher -> SomeQuery (QueryElement SkillMatcher)
  StoryQuery :: StoryMatcher -> SomeQuery (QueryElement StoryMatcher)
  EventQuery :: EventMatcher -> SomeQuery (QueryElement EventMatcher)
  EffectQuery :: EffectMatcher -> SomeQuery (QueryElement EffectMatcher)
  ActQuery :: ActMatcher -> SomeQuery (QueryElement ActMatcher)
  AgendaQuery :: AgendaMatcher -> SomeQuery (QueryElement AgendaMatcher)
  ScenarioQuery :: ScenarioMatcher -> SomeQuery (QueryElement ScenarioMatcher)
  CampaignQuery :: CampaignMatcher -> SomeQuery (QueryElement CampaignMatcher)
  RemainingActQuery :: RemainingActMatcher -> SomeQuery (QueryElement RemainingActMatcher)
  CardQuery :: CardMatcher -> SomeQuery (QueryElement CardMatcher)
  ChaosTokenQuery :: ChaosTokenMatcher -> SomeQuery (QueryElement ChaosTokenMatcher)
  TargetQuery :: TargetMatcher -> SomeQuery (QueryElement TargetMatcher)
  SourceQuery :: SourceMatcher -> SomeQuery (QueryElement SourceMatcher)

deriving stock instance Show (SomeQuery a)
deriving stock instance Eq (SomeQuery a)
deriving stock instance Ord (SomeQuery a)
