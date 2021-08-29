module Arkham.Types.Location.Cards.ArkhamWoodsCliffside where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (arkhamWoodsCliffside)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Message
import Arkham.Types.SkillType

newtype ArkhamWoodsCliffside = ArkhamWoodsCliffside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities env)

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

-- TODO: Move this to a modifier
instance LocationRunner env => RunMessage env ArkhamWoodsCliffside where
  runMessage msg (ArkhamWoodsCliffside attrs@LocationAttrs {..}) = case msg of
    Investigate iid lid s mt _ False | lid == locationId -> do
      let investigate = Investigate iid lid s mt SkillAgility False
      ArkhamWoodsCliffside <$> runMessage investigate attrs
    _ -> ArkhamWoodsCliffside <$> runMessage msg attrs
