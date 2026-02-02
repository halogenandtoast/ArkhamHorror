module Arkham.Helpers.Source where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Field.Import
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.FetchCard
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.Ref
import Arkham.Id
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Story.Types (Field (..))
import Arkham.Tracing
import Arkham.Trait (Trait, toTraits)

sourceTraits :: (HasCallStack, HasGame m, Tracing m) => Source -> m (Set Trait)
sourceTraits = \case
  PaymentSource s -> sourceTraits s
  UseAbilitySource _ s _ -> sourceTraits s
  AbilitySource s _ -> sourceTraits s
  DiscoverSource _ -> pure mempty
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
  BothSource left right -> (<>) <$> sourceTraits left <*> sourceTraits right
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
  ScarletKeySource _ -> pure mempty
  ConcealedCardSource _ -> pure mempty

getSourceController :: (HasGame m, Tracing m) => Source -> m (Maybe InvestigatorId)
getSourceController = \case
  AbilitySource s _ -> getSourceController s
  UseAbilitySource iid _ _ -> pure $ Just iid
  AssetSource aid -> selectAssetController aid
  EventSource eid -> selectEventController eid
  SkillSource sid -> selectSkillController sid
  InvestigatorSource iid -> pure $ Just iid
  _ -> pure Nothing

sourceMatches :: (HasCallStack, HasGame m, Tracing m) => Source -> Matcher.SourceMatcher -> m Bool
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
  Matcher.SourceIsTreacheryEffect tm -> case s.treachery of
    Just tid -> case tm of
      Matcher.AnyTreachery -> pure True
      _ -> elem tid <$> select tm
    Nothing -> pure False
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
        AbilitySource source' n -> do
          iid' <- getActiveInvestigatorId
          checkSource (UseAbilitySource iid' source' n)
        UseAbilitySource iid' source' _ -> orM [elem iid' <$> select whoMatcher, checkSource source']
        AssetSource aid ->
          selectAssetController aid >>= \case
            Just iid' -> elem iid' <$> select whoMatcher
            _ -> pure False
        EventSource eid -> do
          selectEventController eid >>= \case
            Just controllerId -> elem controllerId <$> select whoMatcher
            Nothing -> do
              -- event may have been discarded already
              mOwner <- (toCardOwner =<<) <$> fieldMay EventCard eid
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
        DiscoverSource {} -> False
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
        ScarletKeySource {} -> True
        ConcealedCardSource {} -> True
        PaymentSource {} -> False
    pure $ go s
  Matcher.SourceIsType t -> member t <$> sourceTypes s
  Matcher.EncounterCardSource ->
    let
      check = \case
        AbilitySource source' _ -> check source'
        UseAbilitySource _ source' _ -> check source'
        AssetSource aid -> matches aid (Matcher.AssetCardMatch Matcher.IsEncounterCard <> Matcher.UncontrolledAsset)
        ActSource _ -> pure True
        AgendaSource _ -> pure True
        EnemySource _ -> pure True
        LocationSource _ -> pure True
        TreacherySource _ -> pure True
        StorySource _ -> pure True
        ProxySource k _ -> check k
        ChaosTokenSource _ -> pure True
        ChaosTokenEffectSource _ -> pure True
        _ -> pure False
     in
      check s
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
        ProxySource k _ -> check k
        ChaosTokenSource _ -> True
        ChaosTokenEffectSource _ -> True
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

sourceTypes :: (HasCallStack, Tracing m, HasGame m) => Source -> m (Set CardType)
sourceTypes = \case
  PaymentSource s -> sourceTypes s
  UseAbilitySource _ s _ -> sourceTypes s
  AbilitySource s _ -> sourceTypes s
  CardIdSource c -> do
    fetchCardMaybe_ c <&> \case
      Just card -> singleton card.kind
      Nothing -> mempty
  ProxySource s _ -> sourceTypes s
  IndexedSource _ s -> sourceTypes s
  BothSource l r -> (<>) <$> sourceTypes l <*> sourceTypes r
  AssetSource aid ->
    fetchCardMaybe_ aid <&> \case
      Just card -> singleton card.kind
      Nothing -> setFromList [AssetType, EncounterAssetType]
  EventSource eid ->
    fetchCardMaybe_ eid <&> \case
      Just card -> singleton card.kind
      Nothing -> setFromList [EventType, EncounterEventType]
  SkillSource _ -> pure $ singleton SkillType
  TreacherySource tid ->
    fetchCardMaybe_ tid <&> \case
      Just card -> singleton card.kind
      Nothing -> setFromList [TreacheryType, PlayerTreacheryType]
  EnemySource eid ->
    fetchCardMaybe_ eid <&> \case
      Just card -> singleton card.kind
      Nothing -> setFromList [EnemyType, PlayerEnemyType]
  LocationSource _ -> pure $ singleton LocationType
  ActSource _ -> pure $ singleton ActType
  AgendaSource _ -> pure $ singleton AgendaType
  StorySource _ -> pure $ singleton StoryType
  InvestigatorSource _ -> pure $ singleton InvestigatorType
  ScenarioSource -> pure $ singleton ScenarioType
  ScarletKeySource _ -> pure $ singleton KeyType
  _ -> pure mempty
