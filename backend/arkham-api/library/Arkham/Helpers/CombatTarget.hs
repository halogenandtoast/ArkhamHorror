module Arkham.Helpers.CombatTarget where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Asset)
import Arkham.Campaigns.TheScarletKeys.Concealed.Query (ForExpose (..), getConcealedAt)
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Id
import Arkham.Matcher
import Arkham.Modifier (ModifierType (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Taboo

{- | Extra locations an attacking asset's own text adds to the standard range of
its attacks. Springfield M1903's tabooed ability targets "up to one location away
from its standard range", so a card that /sets/ that range -- Telescopic Sight (3),
Marksmanship (1) -- adds to this rather than replacing it.

The read has to be a direct query: 'getModifiers' inside 'getModifiersFor' sees the
previous 'preloadModifiers' snapshot.
-}
getAttackRangeBonus :: HasGame m => AssetId -> m Int
getAttackRangeBonus aid = do
  attrs <- getAttrs @Asset aid
  pure
    $ if attrs.cardCode == Assets.springfieldM19034.cardCode && tabooed TabooList19 attrs
      then 1
      else 0

data FightTarget
  = FightTargetEnemy EnemyId
  | FightTargetConcealed ConcealedCardId
  | FightTargetLocation LocationId
  | FightTargetAsset AssetId

data EvadeTarget
  = EvadeTargetEnemy EnemyId
  | EvadeTargetConcealed ConcealedCardId

getLocalConcealedIds :: HasGame m => InvestigatorId -> m [ConcealedCardId]
getLocalConcealedIds =
  getLocationOf >=> \case
    Nothing -> pure []
    Just loc -> map (.id) <$> getConcealedAt NotForExpose loc

getFightTargets :: HasGame m => Source -> InvestigatorId -> m [FightTarget]
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

getEvadeTargets :: HasGame m => Source -> InvestigatorId -> m [EvadeTarget]
getEvadeTargets source iid = do
  enemies <- map EvadeTargetEnemy <$> select (CanEvadeEnemy source)
  concealed <- map EvadeTargetConcealed <$> getLocalConcealedIds iid
  pure $ enemies <> concealed

hasFightTargets :: HasGame m => Source -> InvestigatorId -> m Bool
hasFightTargets source iid = notNull <$> getFightTargets source iid

hasEvadeTargets :: HasGame m => Source -> InvestigatorId -> m Bool
hasEvadeTargets source iid = notNull <$> getEvadeTargets source iid
