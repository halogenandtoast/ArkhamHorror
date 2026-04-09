module Arkham.Helpers.CombatTarget where

import Arkham.Campaigns.TheScarletKeys.Concealed.Query (ForExpose (..), getConcealedAt)
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Id
import Arkham.Matcher
import Arkham.Modifier (ModifierType (..))
import Arkham.Prelude
import Arkham.Source
import Arkham.Tracing

data FightTarget
  = FightTargetEnemy EnemyId
  | FightTargetConcealed ConcealedCardId
  | FightTargetLocation LocationId
  | FightTargetAsset AssetId

data EvadeTarget
  = EvadeTargetEnemy EnemyId
  | EvadeTargetConcealed ConcealedCardId

getLocalConcealedIds :: (HasGame m, Tracing m) => InvestigatorId -> m [ConcealedCardId]
getLocalConcealedIds =
  getLocationOf >=> \case
    Nothing -> pure []
    Just loc -> map (.id) <$> getConcealedAt NotForExpose loc

getFightTargets :: (HasGame m, Tracing m) => Source -> InvestigatorId -> m [FightTarget]
getFightTargets source iid = do
  enemies <- map FightTargetEnemy <$> select (CanFightEnemy source)
  concealed <- map FightTargetConcealed <$> getLocalConcealedIds iid
  locations <-
    map FightTargetLocation
      <$> select (LocationWithModifier CanBeAttackedAsIfEnemy <> locationWithInvestigator iid)
  assets <-
    map FightTargetAsset
      <$> select (AssetWithModifier CanBeAttackedAsIfEnemy <> at_ (locationWithInvestigator iid))
  pure $ enemies <> concealed <> locations <> assets

getEvadeTargets :: (HasGame m, Tracing m) => Source -> InvestigatorId -> m [EvadeTarget]
getEvadeTargets source iid = do
  enemies <- map EvadeTargetEnemy <$> select (CanEvadeEnemy source)
  concealed <- map EvadeTargetConcealed <$> getLocalConcealedIds iid
  pure $ enemies <> concealed

hasFightTargets :: (HasGame m, Tracing m) => Source -> InvestigatorId -> m Bool
hasFightTargets source iid = notNull <$> getFightTargets source iid

hasEvadeTargets :: (HasGame m, Tracing m) => Source -> InvestigatorId -> m Bool
hasEvadeTargets source iid = notNull <$> getEvadeTargets source iid
