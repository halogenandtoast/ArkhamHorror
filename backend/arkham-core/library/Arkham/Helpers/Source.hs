module Arkham.Helpers.Source where

import Arkham.Prelude

import Arkham.Field.Import
import Arkham.Projection
import Arkham.Card.CardType
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Matcher qualified as Matcher
import Arkham.Source
import Arkham.Trait ( Trait, toTraits )

sourceTraits :: (HasCallStack, HasGame m) => Source -> m (Set Trait)
sourceTraits = \case
  AbilitySource s _ -> sourceTraits s
  ActDeckSource -> pure mempty
  ActSource _ -> pure mempty
  AfterSkillTestSource -> pure mempty
  AgendaDeckSource -> pure mempty
  AgendaSource _ -> pure mempty
  AgendaMatcherSource _ -> pure mempty
  AssetMatcherSource _ -> pure mempty
  AssetSource aid -> field AssetTraits aid

  CardCodeSource _ -> pure mempty
  CardSource c -> pure (toTraits c)
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
  ResourceSource -> pure mempty

  ScenarioSource -> pure mempty
  SkillSource sid -> field SkillTraits sid
  SkillTestSource{} -> pure mempty
  TreacherySource tid -> field TreacheryTraits tid

  StorySource _ -> pure mempty
  TestSource traits -> pure traits
  TokenEffectSource _ -> pure mempty
  TokenSource _ -> pure mempty
  YouSource -> selectJust Matcher.You >>= field InvestigatorTraits
  LocationMatcherSource _ -> pure mempty
  EnemyMatcherSource _ -> pure mempty
  CampaignSource -> pure mempty

  ThisCard -> error "can not get traits"
  CardCostSource _ -> pure mempty
  BothSource _ _ -> error "doesn't make sense, or will solve later"

getSourceController :: HasGame m => Source -> m (Maybe InvestigatorId)
getSourceController = \case
  AssetSource aid -> selectAssetController aid
  EventSource eid -> selectEventController eid
  SkillSource sid -> selectSkillController sid
  InvestigatorSource iid -> pure $ Just iid
  _ -> pure Nothing

sourceMatches
  :: (HasCallStack, HasGame m) => Source -> Matcher.SourceMatcher -> m Bool
sourceMatches s = \case
  Matcher.SourceIsCancelable sm -> case s of
    CardCostSource _ -> pure False
    _ -> sourceMatches s sm
  Matcher.SourceIs s' -> pure $ s == s'
  Matcher.NotSource matcher -> not <$> sourceMatches s matcher
  Matcher.SourceMatchesAny ms -> anyM (sourceMatches s) ms
  Matcher.SourceWithTrait t -> elem t <$> sourceTraits s
  Matcher.SourceIsEnemyAttack em -> case s of
    EnemyAttackSource eid -> member eid <$> select em
    _ -> pure False
  Matcher.AnySource -> pure True
  Matcher.SourceMatches ms -> allM (sourceMatches s) ms
  Matcher.SourceOwnedBy whoMatcher ->
    let
      checkSource = \case
        AbilitySource source' _ -> checkSource source'
        AssetSource aid -> do
          mControllerId <- selectAssetController aid
          case mControllerId of
            Just iid' -> member iid' <$> select whoMatcher
            _ -> pure False
        EventSource eid -> do
          mControllerId <- selectEventController eid
          case mControllerId of
            Just controllerId -> member controllerId <$> select whoMatcher
            Nothing -> pure False
        SkillSource sid -> do
          mControllerId <- selectSkillController sid
          case mControllerId of
            Just controllerId -> member controllerId <$> select whoMatcher
            Nothing -> pure False
        InvestigatorSource iid -> member iid <$> select whoMatcher
        _ -> pure False
    in checkSource s
  Matcher.SourceIsType t -> pure $ case t of
    AssetType -> case s of
      AssetSource _ -> True
      _ -> False
    EventType -> case s of
      EventSource _ -> True
      _ -> False
    SkillType -> case s of
      SkillSource _ -> True
      _ -> False
    PlayerTreacheryType -> case s of
      TreacherySource _ -> True
      _ -> False
    PlayerEnemyType -> case s of
      EnemySource _ -> True
      _ -> False
    TreacheryType -> case s of
      TreacherySource _ -> True
      _ -> False
    EnemyType -> case s of
      EnemySource _ -> True
      _ -> False
    LocationType -> case s of
      LocationSource _ -> True
      _ -> False
    EncounterAssetType -> case s of
      AssetSource _ -> True
      _ -> False
    ActType -> case s of
      ActSource _ -> True
      _ -> False
    AgendaType -> case s of
      AgendaSource _ -> True
      _ -> False
    StoryType -> case s of
      StorySource _ -> True
      _ -> False
    InvestigatorType -> case s of
      InvestigatorSource _ -> True
      _ -> False
    ScenarioType -> case s of
      ScenarioSource -> True
      _ -> False
  Matcher.EncounterCardSource -> pure $ case s of
    ActSource _ -> True
    AgendaSource _ -> True
    EnemySource _ -> True
    LocationSource _ -> True
    TreacherySource _ -> True
    _ -> False

