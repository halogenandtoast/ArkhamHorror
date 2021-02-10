module Arkham.Types.Location.Cards.ArkhamWoodsOldHouse where

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

newtype ArkhamWoodsOldHouse = ArkhamWoodsOldHouse LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsOldHouse :: ArkhamWoodsOldHouse
arkhamWoodsOldHouse = ArkhamWoodsOldHouse $ base
  { locationRevealedConnectedSymbols = setFromList [Squiggle, Triangle, T]
  , locationRevealedSymbol = Diamond
  }
 where
  base = baseAttrs
    "01152"
    (Name "Arkham Woods" (Just "Old House"))
    EncounterSet.TheDevourerBelow
    2
    (PerPlayer 1)
    Square
    [Squiggle]
    [Woods]

instance HasModifiersFor env ArkhamWoodsOldHouse where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env ArkhamWoodsOldHouse where
  getActions i window (ArkhamWoodsOldHouse attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsOldHouse where
  runMessage msg (ArkhamWoodsOldHouse attrs@LocationAttrs {..}) = case msg of
    Investigate iid lid s _ False | lid == locationId -> do
      let investigate = Investigate iid lid s SkillWillpower False
      ArkhamWoodsOldHouse <$> runMessage investigate attrs
    _ -> ArkhamWoodsOldHouse <$> runMessage msg attrs
