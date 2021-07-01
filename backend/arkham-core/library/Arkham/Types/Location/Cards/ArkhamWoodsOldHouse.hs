module Arkham.Types.Location.Cards.ArkhamWoodsOldHouse where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (arkhamWoodsOldHouse)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.SkillType

newtype ArkhamWoodsOldHouse = ArkhamWoodsOldHouse LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsOldHouse :: LocationId -> ArkhamWoodsOldHouse
arkhamWoodsOldHouse =
  ArkhamWoodsOldHouse
    . (revealedConnectedSymbolsL .~ setFromList [Squiggle, Triangle, T])
    . (revealedSymbolL .~ Diamond)
    . baseAttrs
        Cards.arkhamWoodsOldHouse
        2
        (PerPlayer 1)
        Square
        [Squiggle]

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
