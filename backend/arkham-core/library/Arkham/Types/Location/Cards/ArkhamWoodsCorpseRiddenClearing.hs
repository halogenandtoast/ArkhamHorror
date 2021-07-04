module Arkham.Types.Location.Cards.ArkhamWoodsCorpseRiddenClearing where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
  (arkhamWoodsCorpseRiddenClearing)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype ArkhamWoodsCorpseRiddenClearing = ArkhamWoodsCorpseRiddenClearing LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsCorpseRiddenClearing :: LocationCard ArkhamWoodsCorpseRiddenClearing
arkhamWoodsCorpseRiddenClearing = locationWith
  ArkhamWoodsCorpseRiddenClearing
  Cards.arkhamWoodsCorpseRiddenClearing
  3
  (PerPlayer 1)
  Square
  [Squiggle]
  ((revealedConnectedSymbolsL .~ setFromList [Squiggle, Circle])
  . (revealedSymbolL .~ Droplet)
  )

instance HasModifiersFor env ArkhamWoodsCorpseRiddenClearing where
  getModifiersFor _ (EnemyTarget eid) (ArkhamWoodsCorpseRiddenClearing attrs) =
    pure $ toModifiers
      attrs
      [ MaxDamageTaken 1 | eid `elem` locationEnemies attrs ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env ArkhamWoodsCorpseRiddenClearing where
  getActions i window (ArkhamWoodsCorpseRiddenClearing attrs) =
    getActions i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsCorpseRiddenClearing where
  runMessage msg (ArkhamWoodsCorpseRiddenClearing attrs) =
    ArkhamWoodsCorpseRiddenClearing <$> runMessage msg attrs
