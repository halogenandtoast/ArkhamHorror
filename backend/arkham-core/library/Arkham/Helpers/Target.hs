module Arkham.Helpers.Target where

import Arkham.Asset.Types (Field (..))
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Types (Field (..))
import {-# SOURCE #-} Arkham.Game ()
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
  ActDeckTarget -> pure mempty
  ActTarget _ -> pure mempty
  AfterSkillTestTarget -> pure mempty
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
  ResourceTarget -> pure mempty
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
