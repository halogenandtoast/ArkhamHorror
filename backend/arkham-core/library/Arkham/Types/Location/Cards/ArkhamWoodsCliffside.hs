module Arkham.Types.Location.Cards.ArkhamWoodsCliffside where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (arkhamWoodsCliffside)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.SkillType

newtype ArkhamWoodsCliffside = ArkhamWoodsCliffside LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsCliffside :: LocationCard ArkhamWoodsCliffside
arkhamWoodsCliffside = locationWith
  ArkhamWoodsCliffside
  Cards.arkhamWoodsCliffside
  2
  (PerPlayer 1)
  Square
  [Squiggle]
  ((revealedConnectedSymbolsL .~ setFromList [Squiggle, Moon, Triangle])
  . (revealedSymbolL .~ Hourglass)
  )

instance HasModifiersFor env ArkhamWoodsCliffside

instance ActionRunner env => HasAbilities env ArkhamWoodsCliffside where
  getAbilities i window (ArkhamWoodsCliffside attrs) = getAbilities i window attrs

instance (LocationRunner env) => RunMessage env ArkhamWoodsCliffside where
  runMessage msg (ArkhamWoodsCliffside attrs@LocationAttrs {..}) = case msg of
    Investigate iid lid s _ False | lid == locationId -> do
      let investigate = Investigate iid lid s SkillAgility False
      ArkhamWoodsCliffside <$> runMessage investigate attrs
    _ -> ArkhamWoodsCliffside <$> runMessage msg attrs
