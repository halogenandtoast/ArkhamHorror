module Arkham.Types.Location.Cards.ArkhamWoodsCorpseRiddenClearing where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Target
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ArkhamWoodsCorpseRiddenClearing = ArkhamWoodsCorpseRiddenClearing LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsCorpseRiddenClearing :: ArkhamWoodsCorpseRiddenClearing
arkhamWoodsCorpseRiddenClearing = ArkhamWoodsCorpseRiddenClearing $ base
  { locationRevealedConnectedSymbols = setFromList [Squiggle, Circle]
  , locationRevealedSymbol = Droplet
  }
 where
  base = baseAttrs
    "50035"
    (Name "Arkham Woods" (Just "Corpse-Ridden Clearing"))
    EncounterSet.ReturnToTheDevourerBelow
    3
    (PerPlayer 1)
    Square
    [Squiggle]
    [Woods]

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
