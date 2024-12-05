module Arkham.Location.Cards.ArkhamWoodsCorpseRiddenClearing where

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (arkhamWoodsCorpseRiddenClearing)
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ArkhamWoodsCorpseRiddenClearing = ArkhamWoodsCorpseRiddenClearing LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

arkhamWoodsCorpseRiddenClearing :: LocationCard ArkhamWoodsCorpseRiddenClearing
arkhamWoodsCorpseRiddenClearing =
  location ArkhamWoodsCorpseRiddenClearing Cards.arkhamWoodsCorpseRiddenClearing 3 (PerPlayer 1)

instance HasModifiersFor ArkhamWoodsCorpseRiddenClearing where
  getModifiersFor (ArkhamWoodsCorpseRiddenClearing a) =
    whenRevealed a $ modifySelect a (enemyAt a) [MaxDamageTaken 1]

instance RunMessage ArkhamWoodsCorpseRiddenClearing where
  runMessage msg (ArkhamWoodsCorpseRiddenClearing attrs) =
    ArkhamWoodsCorpseRiddenClearing <$> runMessage msg attrs
