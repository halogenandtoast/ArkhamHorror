module Arkham.Helpers.Ref where

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Effect.Types (Field (..))
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Types (Field (..))
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Location.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Skill.Types (Field (..))
import Arkham.Source
import Arkham.Story.Types (Field (..))
import Arkham.Target
import Arkham.Treachery.Types (Field (..))

targetToCard :: (HasCallStack, HasGame m) => Target -> m Card
targetToCard target = fromMaybe handleMissing <$> targetToMaybeCard target
 where
  handleMissing = error $ "unhandled: " <> show target

targetToMaybeCard :: (HasCallStack, HasGame m) => Target -> m (Maybe Card)
targetToMaybeCard = \case
  AssetTarget aid -> fieldMay AssetCard aid
  EventTarget aid -> fieldMay EventCard aid
  SkillTarget aid -> fieldMay SkillCard aid
  EnemyTarget aid -> fieldMay EnemyCard aid
  StoryTarget aid -> fieldMay StoryCard aid
  EffectTarget eid -> join <$> fieldMay EffectCard eid
  TreacheryTarget aid -> fieldMay TreacheryCard aid
  LocationTarget aid -> fieldMay LocationCard aid
  SearchedCardTarget cId -> Just <$> getCard cId
  CardIdTarget cId -> Just <$> getCard cId
  BothTarget a b -> do
    aCard <- targetToMaybeCard a
    bCard <- targetToMaybeCard b
    pure $ aCard <|> bCard
  _ -> pure Nothing

sourceToCard :: (HasCallStack, HasGame m) => Source -> m Card
sourceToCard = targetToCard . sourceToTarget

sourceToMaybeCard :: (HasCallStack, HasGame m, Sourceable source) => source -> m (Maybe Card)
sourceToMaybeCard = maybe (pure Nothing) targetToMaybeCard . sourceToMaybeTarget . toSource

sourceToTarget :: HasCallStack => Source -> Target
sourceToTarget s =
  fromJustNote ("trying to convert source " <> show s <> " but can't")
    $ sourceToMaybeTarget s

sourceToMaybeTarget :: Source -> Maybe Target
sourceToMaybeTarget = \case
  YouSource -> Just YouTarget
  AssetSource aid -> Just $ AssetTarget aid
  EnemySource eid -> Just $ EnemyTarget eid
  CardIdSource c -> Just $ CardIdTarget c
  ScenarioSource -> Just $ ScenarioTarget
  InvestigatorSource iid -> Just $ InvestigatorTarget iid
  CardCodeSource cid -> Just $ CardCodeTarget cid
  ChaosTokenSource t -> Just $ ChaosTokenTarget t
  ChaosTokenEffectSource _ -> Nothing
  ElderSignEffectSource _ -> Nothing
  AgendaSource aid -> Just $ AgendaTarget aid
  LocationSource lid -> Just $ LocationTarget lid
  SkillTestSource sid -> Just $ SkillTestTarget sid
  TreacherySource tid -> Just $ TreacheryTarget tid
  EventSource eid -> Just $ EventTarget eid
  SkillSource sid -> Just $ SkillTarget sid
  EmptyDeckSource -> Nothing
  DeckSource -> Nothing
  GameSource -> Just $ GameTarget
  ActSource aid -> Just $ ActTarget aid
  EncounterCardSource _ -> Nothing
  TestSource {} -> Just $ TestTarget
  ProxySource _ source -> sourceToMaybeTarget source
  IndexedSource _ source -> sourceToMaybeTarget source
  EffectSource eid -> Just $ EffectTarget eid
  ResourceSource iid -> Just $ ResourceTarget iid
  AbilitySource s _ -> sourceToMaybeTarget s
  ActDeckSource -> Just $ ActDeckTarget
  AgendaDeckSource -> Just $ AgendaDeckTarget
  ActMatcherSource {} -> Nothing
  AgendaMatcherSource {} -> Nothing
  AssetMatcherSource {} -> Nothing
  LocationMatcherSource {} -> Nothing
  EnemyMatcherSource {} -> Nothing
  EnemyAttackSource a -> Just $ EnemyTarget a
  StorySource code -> Just $ StoryTarget code
  CampaignSource -> Just $ CampaignTarget
  TarotSource arcana -> Just $ TarotTarget arcana
  ThisCard -> Nothing
  CardCostSource a -> Just $ CardCostTarget a
  BothSource s1 s2 -> BothTarget <$> sourceToMaybeTarget s1 <*> sourceToMaybeTarget s2
  BatchSource bId -> Just $ BatchTarget bId
  ActiveCostSource acId -> Just $ ActiveCostTarget acId

targetToSource :: Target -> Source
targetToSource = \case
  GameTarget -> GameSource
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
  SkillTestTarget sid -> SkillTestSource sid
  TreacheryTarget tid -> TreacherySource tid
  EncounterDeckTarget -> error "can not covert"
  ScenarioDeckTarget -> error "can not covert"
  AgendaTarget aid -> AgendaSource aid
  ActTarget aid -> ActSource aid
  CardIdTarget cid -> CardIdSource cid
  CardCostTarget cid -> CardCostSource cid
  CardCodeTarget ccode -> CardCodeSource ccode
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
  StoryTarget code -> StorySource code
  AgendaMatcherTarget _ -> error "can not convert"
  CampaignTarget -> CampaignSource
  TarotTarget arcana -> TarotSource arcana
  AbilityTarget _ _ -> error "can not convert"
  BothTarget t1 t2 -> BothSource (targetToSource t1) (targetToSource t2)
  BatchTarget bId -> BatchSource bId
  ActiveCostTarget acId -> ActiveCostSource acId
  LabeledTarget _ target -> targetToSource target
