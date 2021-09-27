module Arkham.Types.Location.Cards.ArkhamWoodsCorpseRiddenClearing where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
  (arkhamWoodsCorpseRiddenClearing)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype ArkhamWoodsCorpseRiddenClearing = ArkhamWoodsCorpseRiddenClearing LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

arkhamWoodsCorpseRiddenClearing :: LocationCard ArkhamWoodsCorpseRiddenClearing
arkhamWoodsCorpseRiddenClearing = locationWithRevealedSideConnections
  ArkhamWoodsCorpseRiddenClearing
  Cards.arkhamWoodsCorpseRiddenClearing
  3
  (PerPlayer 1)
  Square
  [Squiggle]
  Droplet
  [Squiggle, Circle]

instance HasModifiersFor env ArkhamWoodsCorpseRiddenClearing where
  getModifiersFor _ (EnemyTarget eid) (ArkhamWoodsCorpseRiddenClearing attrs) =
    pure $ toModifiers
      attrs
      [ MaxDamageTaken 1 | eid `elem` locationEnemies attrs ]
  getModifiersFor _ _ _ = pure []

instance (LocationRunner env) => RunMessage env ArkhamWoodsCorpseRiddenClearing where
  runMessage msg (ArkhamWoodsCorpseRiddenClearing attrs) =
    ArkhamWoodsCorpseRiddenClearing <$> runMessage msg attrs
