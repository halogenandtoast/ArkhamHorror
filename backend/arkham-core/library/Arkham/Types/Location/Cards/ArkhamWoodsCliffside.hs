module Arkham.Types.Location.Cards.ArkhamWoodsCliffside where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (arkhamWoodsCliffside)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.SkillType

newtype ArkhamWoodsCliffside = ArkhamWoodsCliffside LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsCliffside :: LocationId -> ArkhamWoodsCliffside
arkhamWoodsCliffside =
  ArkhamWoodsCliffside
    . (revealedConnectedSymbolsL .~ setFromList [Squiggle, Moon, Triangle])
    . (revealedSymbolL .~ Hourglass)
    . baseAttrs
        Cards.arkhamWoodsCliffside
        2
        (PerPlayer 1)
        Square
        [Squiggle]

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
