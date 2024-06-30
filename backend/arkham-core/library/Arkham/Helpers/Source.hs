module Arkham.Helpers.Source where

import Arkham.Prelude

import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Field.Import
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Matcher qualified as Matcher
import Arkham.Projection
import Arkham.Source
import Arkham.Trait (Trait, toTraits)

sourceTraits :: (HasCallStack, HasGame m) => Source -> m (Set Trait)
sourceTraits = \case
  AbilitySource s _ -> sourceTraits s
  ActDeckSource -> pure mempty
  ActSource _ -> pure mempty
  AgendaDeckSource -> pure mempty
  AgendaSource _ -> pure mempty
  AgendaMatcherSource _ -> pure mempty
  AssetMatcherSource _ -> pure mempty
  ActMatcherSource _ -> pure mempty
  AssetSource aid -> field AssetTraits aid
  CardCodeSource _ -> pure mempty
  CardSource c -> pure (toTraits c)
  CardIdSource c -> toTraits <$> getCard c
  DeckSource -> pure mempty
  EffectSource _ -> pure mempty
  EmptyDeckSource -> pure mempty
  EncounterCardSource _ -> pure mempty
  EnemyAttackSource _ -> pure mempty
  EnemySource eid -> field EnemyTraits eid
  EventSource eid -> field EventTraits eid
  GameSource -> pure mempty
  InvestigatorSource iid -> field InvestigatorTraits iid
  LocationSource lid -> field LocationTraits lid
  PlayerCardSource _ -> pure mempty
  ProxySource s _ -> sourceTraits s
  ResourceSource _ -> pure mempty
  ScenarioSource -> pure mempty
  SkillSource sid -> field SkillTraits sid
  SkillTestSource {} -> pure mempty
  TreacherySource tid -> field TreacheryTraits tid
  StorySource _ -> pure mempty
  TestSource traits -> pure traits
  ChaosTokenSource _ -> pure mempty
  ChaosTokenEffectSource _ -> pure mempty
  YouSource -> selectJust Matcher.You >>= field InvestigatorTraits
  LocationMatcherSource _ -> pure mempty
  EnemyMatcherSource _ -> pure mempty
  CampaignSource -> pure mempty
  TarotSource _ -> pure mempty
  ThisCard -> error "can not get traits"
  CardCostSource _ -> pure mempty
  BothSource _ _ -> error "doesn't make sense, or will solve later"
  BatchSource _ -> pure mempty
  ActiveCostSource _ -> pure mempty

getSourceController :: HasGame m => Source -> m (Maybe InvestigatorId)
getSourceController = \case
  AbilitySource s _ -> getSourceController s
  AssetSource aid -> selectAssetController aid
  EventSource eid -> selectEventController eid
  SkillSource sid -> selectSkillController sid
  InvestigatorSource iid -> pure $ Just iid
  _ -> pure Nothing
