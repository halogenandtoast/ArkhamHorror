module Arkham.Query where

import {-# SOURCE #-} Arkham.Ability.Types
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.Id
import Arkham.Matcher
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
