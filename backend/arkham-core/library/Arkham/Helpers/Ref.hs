module Arkham.Helpers.Ref where

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Types (Field (..))
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Location.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Skill.Types (Field (..))
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Types (Field (..))

targetToCard :: (HasCallStack, HasGame m) => Target -> m Card
targetToCard target = fromMaybe handleMissing <$> targetToMaybeCard target
 where
  handleMissing = error $ "unhandled: " <> show target

targetToMaybeCard :: (HasCallStack, HasGame m) => Target -> m (Maybe Card)
targetToMaybeCard = \case
  AssetTarget aid -> Just <$> field AssetCard aid
  EventTarget aid -> Just <$> field EventCard aid
  SkillTarget aid -> Just <$> field SkillCard aid
  EnemyTarget aid -> Just <$> field EnemyCard aid
  TreacheryTarget aid -> Just <$> field TreacheryCard aid
  LocationTarget aid -> Just <$> field LocationCard aid
  CardTarget c -> pure $ Just c
  SearchedCardTarget cId -> Just <$> getCard cId
  CardIdTarget cId -> Just <$> getCard cId
  BothTarget a b -> do
    aCard <- targetToMaybeCard a
    bCard <- targetToMaybeCard b
    pure $ aCard <|> bCard
  _ -> pure Nothing

sourceToCard :: (HasCallStack, HasGame m) => Source -> m Card
sourceToCard = targetToCard . sourceToTarget

sourceToMaybeCard :: (HasCallStack, HasGame m) => Source -> m (Maybe Card)
sourceToMaybeCard = targetToMaybeCard . sourceToTarget

sourceToTarget :: Source -> Target
sourceToTarget = \case
  YouSource -> YouTarget
  AssetSource aid -> AssetTarget aid
  EnemySource eid -> EnemyTarget eid
  CardSource c -> CardTarget c
  CardIdSource c -> CardIdTarget c
  ScenarioSource -> ScenarioTarget
  InvestigatorSource iid -> InvestigatorTarget iid
  CardCodeSource cid -> CardCodeTarget cid
  ChaosTokenSource t -> ChaosTokenTarget t
  ChaosTokenEffectSource _ -> error "not implemented"
  AgendaSource aid -> AgendaTarget aid
  LocationSource lid -> LocationTarget lid
  SkillTestSource -> SkillTestTarget
  TreacherySource tid -> TreacheryTarget tid
  EventSource eid -> EventTarget eid
  SkillSource sid -> SkillTarget sid
  EmptyDeckSource -> error "not implemented"
  DeckSource -> error "not implemented"
  GameSource -> error "not implemented"
  ActSource aid -> ActTarget aid
  PlayerCardSource _ -> error "not implemented"
  EncounterCardSource _ -> error "not implemented"
  TestSource {} -> TestTarget
  ProxySource _ source -> sourceToTarget source
  EffectSource eid -> EffectTarget eid
  ResourceSource iid -> ResourceTarget iid
  AbilitySource s _ -> sourceToTarget s
  ActDeckSource -> ActDeckTarget
  AgendaDeckSource -> AgendaDeckTarget
  ActMatcherSource {} -> error "not converted"
  AgendaMatcherSource {} -> error "not converted"
  AssetMatcherSource {} -> error "not converted"
  LocationMatcherSource {} -> error "not converted"
  EnemyMatcherSource {} -> error "not converted"
  EnemyAttackSource a -> EnemyTarget a
  StorySource code -> StoryTarget code
  CampaignSource -> CampaignTarget
  TarotSource arcana -> TarotTarget arcana
  ThisCard -> error "not converted"
  CardCostSource _ -> error "not converted"
  BothSource s1 s2 -> BothTarget (sourceToTarget s1) (sourceToTarget s2)
  BatchSource bId -> BatchTarget bId
  ActiveCostSource acId -> ActiveCostTarget acId

targetToSource :: Target -> Source
targetToSource = \case
  ThisTarget -> error "not converted"
  InvestigatorTarget iid -> InvestigatorSource iid
  InvestigatorHandTarget iid -> InvestigatorSource iid
  InvestigatorDiscardTarget iid -> InvestigatorSource iid
  AssetTarget aid -> AssetSource aid
  EnemyTarget eid -> EnemySource eid
  ScenarioTarget -> ScenarioSource
  EffectTarget eid -> EffectSource eid
  PhaseTarget _ -> error "no need"
  LocationTarget lid -> LocationSource lid
  (SetAsideLocationsTarget _) -> error "can not convert"
  SkillTestTarget -> SkillTestSource
  TreacheryTarget tid -> TreacherySource tid
  EncounterDeckTarget -> error "can not covert"
  ScenarioDeckTarget -> error "can not covert"
  AgendaTarget aid -> AgendaSource aid
  ActTarget aid -> ActSource aid
  CardIdTarget _ -> error "can not convert"
  CardCodeTarget _ -> error "can not convert"
  SearchedCardTarget _ -> error "can not convert"
  EventTarget eid -> EventSource eid
  SkillTarget sid -> SkillSource sid
  SkillTestInitiatorTarget _ -> error "can not convert"
  ChaosTokenTarget _ -> error "not convertable"
  ChaosTokenFaceTarget _ -> error "Not convertable"
  TestTarget -> TestSource mempty
  ResourceTarget iid -> ResourceSource iid
  ActDeckTarget -> ActDeckSource
  AgendaDeckTarget -> AgendaDeckSource
  InvestigationTarget {} -> error "not converted"
  YouTarget -> YouSource
  ProxyTarget {} -> error "can not convert"
  CardTarget {} -> error "can not convert"
  StoryTarget code -> StorySource code
  AgendaMatcherTarget _ -> error "can not convert"
  CampaignTarget -> CampaignSource
  TarotTarget arcana -> TarotSource arcana
  AbilityTarget _ _ -> error "can not convert"
  BothTarget t1 t2 -> BothSource (targetToSource t1) (targetToSource t2)
  BatchTarget bId -> BatchSource bId
  ActiveCostTarget acId -> ActiveCostSource acId
  LabeledTarget _ target -> targetToSource target
