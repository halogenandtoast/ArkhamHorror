module Arkham.Location.Cards.ArkhamWoodsCliffside where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (arkhamWoodsCliffside)
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Message
import Arkham.SkillType

newtype ArkhamWoodsCliffside = ArkhamWoodsCliffside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

arkhamWoodsCliffside :: LocationCard ArkhamWoodsCliffside
arkhamWoodsCliffside = locationWithRevealedSideConnections
  ArkhamWoodsCliffside
  Cards.arkhamWoodsCliffside
  2
  (PerPlayer 1)
  Square
  [Squiggle]
  Hourglass
  [Squiggle, Moon, Triangle]

-- TODO: Move this to a modifier
instance LocationRunner env => RunMessage env ArkhamWoodsCliffside where
  runMessage msg (ArkhamWoodsCliffside attrs@LocationAttrs {..}) = case msg of
    Investigate iid lid s mt _ False | lid == locationId -> do
      let investigate = Investigate iid lid s mt SkillAgility False
      ArkhamWoodsCliffside <$> runMessage investigate attrs
    _ -> ArkhamWoodsCliffside <$> runMessage msg attrs
