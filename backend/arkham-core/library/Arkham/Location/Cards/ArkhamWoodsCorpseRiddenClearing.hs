module Arkham.Location.Cards.ArkhamWoodsCorpseRiddenClearing where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
  ( arkhamWoodsCorpseRiddenClearing )
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype ArkhamWoodsCorpseRiddenClearing = ArkhamWoodsCorpseRiddenClearing LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

arkhamWoodsCorpseRiddenClearing :: LocationCard ArkhamWoodsCorpseRiddenClearing
arkhamWoodsCorpseRiddenClearing = location
  ArkhamWoodsCorpseRiddenClearing
  Cards.arkhamWoodsCorpseRiddenClearing
  3
  (PerPlayer 1)

instance HasModifiersFor ArkhamWoodsCorpseRiddenClearing where
  getModifiersFor (EnemyTarget eid) (ArkhamWoodsCorpseRiddenClearing attrs) =
    pure $ toModifiers
      attrs
      [ MaxDamageTaken 1 | eid `elem` locationEnemies attrs ]
  getModifiersFor _ _ = pure []

instance RunMessage ArkhamWoodsCorpseRiddenClearing where
  runMessage msg (ArkhamWoodsCorpseRiddenClearing attrs) =
    ArkhamWoodsCorpseRiddenClearing <$> runMessage msg attrs
