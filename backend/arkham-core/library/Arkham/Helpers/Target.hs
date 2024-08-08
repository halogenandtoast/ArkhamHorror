module Arkham.Helpers.Target where

import Arkham.Agenda.Types (Field (..))
import Arkham.Asset.Types (Field (..))
import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Types (Field (..))
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Location
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Skill.Types (Field (..))
import Arkham.Target
import Arkham.Trait (HasTraits (..), Trait)
import Arkham.Treachery.Types (Field (..))

targetTraits :: (HasCallStack, HasGame m) => Target -> m (Set Trait)
targetTraits = \case
  GameTarget -> pure mempty
  ActDeckTarget -> pure mempty
  ActTarget _ -> pure mempty
  AgendaDeckTarget -> pure mempty
  AgendaTarget _ -> pure mempty
  AssetTarget aid -> field AssetTraits aid
  CardCodeTarget _ -> pure mempty
  CardIdTarget _ -> pure mempty
  EffectTarget _ -> pure mempty
  EnemyTarget eid -> field EnemyTraits eid
  EventTarget eid -> field EventTraits eid
  InvestigatorTarget iid -> field InvestigatorTraits iid
  LocationTarget lid ->
    selectOne (LocationWithId lid) >>= \case
      Nothing -> pure mempty
      Just _ -> field LocationTraits lid
  ProxyTarget t _ -> targetTraits t
  ResourceTarget _ -> pure mempty
  ScenarioTarget -> pure mempty
  SkillTarget sid -> field SkillTraits sid
  SkillTestTarget {} -> pure mempty
  TreacheryTarget tid -> field TreacheryTraits tid
  StoryTarget _ -> pure mempty
  TestTarget -> pure mempty
  ChaosTokenTarget _ -> pure mempty
  YouTarget -> selectJust You >>= field InvestigatorTraits
  InvestigatorHandTarget _ -> pure mempty
  InvestigatorDiscardTarget _ -> pure mempty
  SetAsideLocationsTarget _ -> pure mempty
  EncounterDeckTarget -> pure mempty
  ScenarioDeckTarget -> pure mempty
  CardTarget c -> pure $ toTraits c
  SearchedCardTarget _ -> pure mempty
  SkillTestInitiatorTarget _ -> pure mempty
  PhaseTarget _ -> pure mempty
  ChaosTokenFaceTarget _ -> pure mempty
  InvestigationTarget _ _ -> pure mempty
  AgendaMatcherTarget _ -> pure mempty
  CampaignTarget -> pure mempty
  TarotTarget _ -> pure mempty
  AbilityTarget _ _ -> pure mempty
  BothTarget _ _ -> error "won't make sense, or need to determine later"
  BatchTarget {} -> pure mempty
  ActiveCostTarget {} -> pure mempty
  LabeledTarget _ t -> targetTraits t
  ThisTarget -> pure mempty

targetMatches :: forall m. HasGame m => Target -> TargetMatcher -> m Bool
targetMatches s = \case
  NotTarget inner -> not <$> targetMatches s inner
  TargetWithTrait t -> (t `member`) <$> targetTraits s
  TargetMatchesAny ms -> anyM (targetMatches s) ms
  TargetIs s' -> pure $ s == s'
  TargetAtLocation ls -> do
    locations <- select ls
    let
      isLocation
        :: forall a item. (EntityId a ~ item, Projection a) => Field a (Maybe LocationId) -> item -> m Bool
      isLocation fld item = fieldMap fld (maybe False (`elem` locations)) item
    case s of
      AssetTarget aid -> isLocation AssetLocation aid
      InvestigatorTarget iid -> isLocation InvestigatorLocation iid
      EnemyTarget eid -> isLocation EnemyLocation eid
      LocationTarget lid -> pure $ lid `elem` locations
      TreacheryTarget tid -> isLocation TreacheryLocation tid
      EventTarget eid -> fieldMapM EventPlacement placementLocation eid <&> maybe False (`elem` locations)
      _ -> pure False
  TargetWithDoom -> case s of
    AssetTarget aid -> fieldSome AssetDoom aid
    InvestigatorTarget iid -> fieldSome InvestigatorDoom iid
    EnemyTarget eid -> fieldSome EnemyDoom eid
    LocationTarget lid -> fieldSome LocationDoom lid
    TreacheryTarget lid -> fieldSome TreacheryDoom lid
    AgendaTarget lid -> fieldSome AgendaDoom lid
    EventTarget lid -> fieldSome EventDoom lid
    _ -> pure False
  AnyTarget -> pure True
  TargetMatches ms -> allM (targetMatches s) ms
  LocationTargetMatches locationMatcher -> case s of
    LocationTarget lid -> lid <=~> locationMatcher
    ProxyTarget proxyTarget _ -> targetMatches proxyTarget (LocationTargetMatches locationMatcher)
    BothTarget left right ->
      orM
        [ targetMatches left (LocationTargetMatches locationMatcher)
        , targetMatches right (LocationTargetMatches locationMatcher)
        ]
    _ -> pure False
  ActTargetMatches actMatcher -> case s of
    ActTarget aid -> aid <=~> actMatcher
    ProxyTarget proxyTarget _ -> targetMatches proxyTarget (ActTargetMatches actMatcher)
    BothTarget left right ->
      orM
        [ targetMatches left (ActTargetMatches actMatcher)
        , targetMatches right (ActTargetMatches actMatcher)
        ]
    _ -> pure False
  AgendaTargetMatches agendaMatcher -> case s of
    AgendaTarget aid -> aid <=~> agendaMatcher
    ProxyTarget proxyTarget _ -> targetMatches proxyTarget (AgendaTargetMatches agendaMatcher)
    BothTarget left right ->
      orM
        [ targetMatches left (AgendaTargetMatches agendaMatcher)
        , targetMatches right (AgendaTargetMatches agendaMatcher)
        ]
    _ -> pure False
  AssetTargetMatches assetMatcher -> case s of
    AssetTarget aid -> aid <=~> assetMatcher
    ProxyTarget proxyTarget _ -> targetMatches proxyTarget (AssetTargetMatches assetMatcher)
    BothTarget left right ->
      orM
        [ targetMatches left (AssetTargetMatches assetMatcher)
        , targetMatches right (AssetTargetMatches assetMatcher)
        ]
    _ -> pure False
  EnemyTargetMatches enemyMatcher -> case s of
    EnemyTarget eid -> eid <=~> enemyMatcher
    ProxyTarget proxyTarget _ -> targetMatches proxyTarget (EnemyTargetMatches enemyMatcher)
    BothTarget left right ->
      orM
        [ targetMatches left (EnemyTargetMatches enemyMatcher)
        , targetMatches right (EnemyTargetMatches enemyMatcher)
        ]
    _ -> pure False
  ScenarioCardTarget -> case s of
    EnemyTarget _ -> pure True
    TreacheryTarget _ -> pure True
    AgendaTarget _ -> pure True
    ActTarget _ -> pure True
    LocationTarget _ -> pure True
    ProxyTarget proxyTarget _ -> targetMatches proxyTarget ScenarioCardTarget
    BothTarget left right ->
      orM [targetMatches left ScenarioCardTarget, targetMatches right ScenarioCardTarget]
    _ -> pure False
