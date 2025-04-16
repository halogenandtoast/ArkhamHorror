module Arkham.Helpers.Source where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Field.Import
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Query
import Arkham.Helpers.Ref
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Story.Types (Field (..))
import Arkham.Trait (Trait, toTraits)

sourceTraits :: (HasCallStack, HasGame m) => Source -> m (Set Trait)
sourceTraits = \case
  UseAbilitySource _ s _ -> sourceTraits s
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
  UseAbilitySource iid _ _ -> pure $ Just iid
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
    other -> do
      modifiers' <- maybe (pure []) getModifiers (sourceToMaybeTarget other)
      andM [pure $ EffectsCannotBeCanceled `notElem` modifiers', sourceMatches s sm]
  Matcher.SourceIs s' -> pure $ s == s'
  Matcher.NotSource matcher -> not <$> sourceMatches s matcher
  Matcher.SourceMatchesAny ms -> anyM (sourceMatches s) ms
  Matcher.SourceWithTrait t -> elem t <$> sourceTraits s
  Matcher.SourceIsEnemyAttack em -> case s of
    EnemyAttackSource eid -> elem eid <$> select em
    _ -> pure False
  Matcher.SourceIsTreacheryEffect tm -> case s of
    TreacherySource tid -> elem tid <$> select tm
    _ -> pure False
  Matcher.SourceIsEnemy em -> case s.enemy of
    Nothing -> pure False
    Just eid -> eid <=~> em
  Matcher.SourceIsLocation lm -> case s.location of
    Nothing -> pure False
    Just lid -> lid <=~> lm
  Matcher.SourceIsAsset am ->
    let
      isAssetSource s' = case s' of
        AssetSource aid -> elem aid <$> select am
        AbilitySource (AssetSource aid) _ -> elem aid <$> select am
        UseAbilitySource _ (AssetSource aid) _ -> elem aid <$> select am
        ProxySource (CardIdSource _) pSource -> isAssetSource pSource
        IndexedSource _ pSource -> isAssetSource pSource
        ProxySource pSource _ -> isAssetSource pSource
        BothSource lSource rSource -> orM [isAssetSource lSource, isAssetSource rSource]
        _ -> pure False
     in
      isAssetSource s
  Matcher.SourceIsEvent am ->
    let
      isEventSource s' = case s' of
        EventSource aid -> elem aid <$> select am
        AbilitySource (EventSource aid) _ -> elem aid <$> select am
        UseAbilitySource _ (EventSource aid) _ -> elem aid <$> select am
        ProxySource (CardIdSource _) pSource -> isEventSource pSource
        ProxySource pSource _ -> isEventSource pSource
        IndexedSource _ pSource -> isEventSource pSource
        BothSource lSource rSource -> orM [isEventSource lSource, isEventSource rSource]
        _ -> pure False
     in
      isEventSource s
  Matcher.AnySource -> pure True
  Matcher.SourceMatches ms -> allM (sourceMatches s) ms
  Matcher.SourceOwnedBy whoMatcher ->
    let
      checkSource = \case
        AbilitySource source' _ -> checkSource source'
        UseAbilitySource _ source' _ -> checkSource source'
        AssetSource aid ->
          selectAssetController aid >>= \case
            Just iid' -> elem iid' <$> select whoMatcher
            _ -> pure False
        EventSource eid -> do
          selectEventController eid >>= \case
            Just controllerId -> elem controllerId <$> select whoMatcher
            Nothing -> do
              -- event may have been discarded already
              mOwner <- join . fmap toCardOwner <$> fieldMay EventCard eid
              case mOwner of
                Just owner -> elem owner <$> select whoMatcher
                Nothing -> pure False
        SkillSource sid -> do
          selectSkillController sid >>= \case
            Just controllerId -> elem controllerId <$> select whoMatcher
            Nothing -> pure False
        InvestigatorSource iid -> elem iid <$> select whoMatcher
        ElderSignEffectSource iid -> elem iid <$> select whoMatcher
        CardIdSource cid -> do
          c <- getCard cid
          case toCardOwner c of
            Nothing -> pure False
            Just iid -> elem iid <$> select whoMatcher
        _ -> pure False
     in
      checkSource s
  Matcher.SourceIsCardEffect -> do
    let
      go = \case
        ChaosTokenSource {} -> True
        ChaosTokenEffectSource {} -> True
        ElderSignEffectSource {} -> True
        ActiveCostSource {} -> False
        AbilitySource {} -> True
        UseAbilitySource {} -> True
        ActSource {} -> True
        ActDeckSource {} -> False
        AgendaDeckSource {} -> False
        AgendaSource {} -> True
        AgendaMatcherSource {} -> True
        AssetMatcherSource {} -> True
        ActMatcherSource {} -> True
        AssetSource {} -> True
        CardCodeSource {} -> False
        CardIdSource {} -> False
        DeckSource {} -> False
        EffectSource {} -> True
        EmptyDeckSource {} -> False
        EncounterCardSource {} -> False
        EnemyAttackSource {} -> True
        EnemySource {} -> True
        EventSource {} -> True
        GameSource -> False
        InvestigatorSource {} -> True
        LocationMatcherSource {} -> True
        EnemyMatcherSource {} -> True
        LocationSource {} -> True
        IndexedSource _ s' -> go s'
        ProxySource (CardIdSource _) s' -> go s'
        ProxySource s' _ -> go s'
        ResourceSource {} -> False
        ScenarioSource {} -> False
        SkillSource {} -> True
        SkillTestSource {} -> False
        StorySource {} -> True
        TestSource {} -> True
        TreacherySource {} -> True
        YouSource {} -> False
        CampaignSource {} -> False
        ThisCard {} -> True
        CardCostSource {} -> False
        BothSource a b -> go a || go b
        TarotSource {} -> True
        BatchSource {} -> False
    pure $ go s
  Matcher.SourceIsType t -> case t of
    AssetType -> case s of
      AssetSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == AssetType
      _ -> pure False
    EventType -> case s of
      EventSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == EventType
      _ -> pure False
    SkillType -> case s of
      SkillSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == SkillType
      _ -> pure False
    PlayerTreacheryType -> case s of
      TreacherySource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == PlayerTreacheryType
      _ -> pure False
    PlayerEnemyType -> case s of
      EnemySource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == PlayerEnemyType
      _ -> pure False
    TreacheryType -> case s of
      TreacherySource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == TreacheryType
      _ -> pure False
    EnemyType -> case s of
      EnemySource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == EnemyType
      _ -> pure False
    LocationType -> case s of
      LocationSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == LocationType
      _ -> pure False
    EncounterAssetType -> case s of
      AssetSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == EncounterAssetType
      _ -> pure False
    EncounterEventType -> case s of
      EventSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == EncounterEventType
      _ -> pure False
    ActType -> case s of
      ActSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == ActType
      _ -> pure False
    AgendaType -> case s of
      AgendaSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == AgendaType
      _ -> pure False
    StoryType -> case s of
      StorySource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == StoryType
      _ -> pure False
    InvestigatorType -> case s of
      InvestigatorSource _ -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == InvestigatorType
      _ -> pure False
    ScenarioType -> case s of
      ScenarioSource -> pure True
      CardIdSource cid -> do
        c <- getCard cid
        pure $ c.kind == ScenarioType
      _ -> pure False
  Matcher.EncounterCardSource ->
    let
      check = \case
        AbilitySource source' _ -> check source'
        UseAbilitySource _ source' _ -> check source'
        ActSource _ -> True
        AgendaSource _ -> True
        EnemySource _ -> True
        LocationSource _ -> True
        TreacherySource _ -> True
        _ -> False
     in
      pure $ check s
  Matcher.SourceIsScenarioCardEffect ->
    let
      check = \case
        AbilitySource source' _ -> check source'
        UseAbilitySource _ source' _ -> check source'
        ActSource _ -> True
        AgendaSource _ -> True
        EnemySource _ -> True
        LocationSource _ -> True
        TreacherySource _ -> True
        StorySource _ -> True
        _ -> False
     in
      pure $ check s
  Matcher.SourceIsPlayerCard ->
    let
      check = \case
        AbilitySource source' _ -> check source'
        UseAbilitySource _ source' _ -> check source'
        AssetSource _ -> True
        EventSource _ -> True
        SkillSource _ -> True
        InvestigatorSource _ -> True
        _ -> False
     in
      pure $ check s
  Matcher.SourceIsPlayerCardAbility ->
    case s of
      AbilitySource s' _ -> sourceMatches s' Matcher.SourceIsPlayerCard
      UseAbilitySource _ s' _ -> sourceMatches s' Matcher.SourceIsPlayerCard
      _ -> pure False
  Matcher.SourceWithCard cardMatcher -> do
    let
      getCardSource = \case
        AbilitySource source' _ -> getCardSource source'
        UseAbilitySource _ source' _ -> getCardSource source'
        AssetSource aid -> fieldMay AssetCard aid
        EventSource eid -> fieldMay EventCard eid
        SkillSource sid -> fieldMay SkillCard sid
        EnemySource eid -> fieldMay EnemyCard eid
        TreacherySource tid -> fieldMay TreacheryCard tid
        LocationSource lid -> fieldMay LocationCard lid
        StorySource sid -> fieldMay StoryCard sid
        InvestigatorSource _ -> pure Nothing
        CardIdSource cid -> Just <$> getCard cid
        _ -> pure Nothing
    mCard <- getCardSource s
    pure $ case mCard of
      Just c -> c `cardMatch` cardMatcher
      Nothing -> False
