module Arkham.Types.Location.Cards.ArkhamWoodsCliffside where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.SkillType
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ArkhamWoodsCliffside = ArkhamWoodsCliffside LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsCliffside :: ArkhamWoodsCliffside
arkhamWoodsCliffside = ArkhamWoodsCliffside $ base
  { locationRevealedConnectedSymbols = setFromList [Squiggle, Moon, Triangle]
  , locationRevealedSymbol = Hourglass
  }
 where
  base = baseAttrs
    "01153"
    (Name "Arkham Woods" (Just "Cliffside"))
    EncounterSet.TheDevourerBelow
    2
    (PerPlayer 1)
    Square
    [Squiggle]
    [Woods]

instance HasModifiersFor env ArkhamWoodsCliffside where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ArkhamWoodsCliffside where
  getActions i window (ArkhamWoodsCliffside attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsCliffside where
  runMessage msg (ArkhamWoodsCliffside attrs@LocationAttrs {..}) = case msg of
    Investigate iid lid s _ False | lid == locationId -> do
      let investigate = Investigate iid lid s SkillAgility False
      ArkhamWoodsCliffside <$> runMessage investigate attrs
    _ -> ArkhamWoodsCliffside <$> runMessage msg attrs
