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
  ActMatcherSource _ -> pure mempty
  ActSource _ -> pure mempty
  ActiveCostSource _ -> pure mempty
  AgendaDeckSource -> pure mempty
  AgendaMatcherSource _ -> pure mempty
  AgendaSource _ -> pure mempty
  AssetMatcherSource _ -> pure mempty
  AssetSource aid -> fromMaybe mempty <$> fieldMay AssetTraits aid
  BatchSource _ -> pure mempty
  BothSource _ _ -> error "doesn't make sense, or will solve later"
  CampaignSource -> pure mempty
  CardCodeSource _ -> pure mempty
  CardCostSource _ -> pure mempty
  CardIdSource c -> toTraits <$> getCard c
  ChaosTokenEffectSource _ -> pure mempty
  ChaosTokenSource _ -> pure mempty
  DeckSource -> pure mempty
  EffectSource _ -> pure mempty
  ElderSignEffectSource {} -> pure mempty
  EmptyDeckSource -> pure mempty
  EncounterCardSource _ -> pure mempty
  EnemyAttackSource _ -> pure mempty
  EnemyMatcherSource _ -> pure mempty
  EnemySource eid -> field EnemyTraits eid
  EventSource eid -> field EventTraits eid
  GameSource -> pure mempty
  InvestigatorSource iid -> field InvestigatorTraits iid
  LocationMatcherSource _ -> pure mempty
  LocationSource lid -> field LocationTraits lid
  ProxySource s _ -> sourceTraits s
  IndexedSource _ s -> sourceTraits s
  ResourceSource _ -> pure mempty
  ScenarioSource -> pure mempty
  SkillSource sid -> field SkillTraits sid
  SkillTestSource {} -> pure mempty
  StorySource _ -> pure mempty
  TarotSource _ -> pure mempty
  TestSource traits -> pure traits
  ThisCard -> error "can not get traits"
  TreacherySource tid -> fromMaybe mempty <$> fieldMay TreacheryTraits tid
  YouSource -> selectJust Matcher.You >>= field InvestigatorTraits

getSourceController :: HasGame m => Source -> m (Maybe InvestigatorId)
getSourceController = \case
  AbilitySource s _ -> getSourceController s
  AssetSource aid -> selectAssetController aid
  EventSource eid -> selectEventController eid
  SkillSource sid -> selectSkillController sid
  InvestigatorSource iid -> pure $ Just iid
  _ -> pure Nothing
