module Arkham.Location.Cards.ArkhamWoodsCliffside (arkhamWoodsCliffside) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ArkhamWoodsCliffside = ArkhamWoodsCliffside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

arkhamWoodsCliffside :: LocationCard ArkhamWoodsCliffside
arkhamWoodsCliffside =
  locationWith
    ArkhamWoodsCliffside
    Cards.arkhamWoodsCliffside
    2
    (PerPlayer 1)
    (investigateSkillL .~ #agility)

instance RunMessage ArkhamWoodsCliffside where
  runMessage msg (ArkhamWoodsCliffside attrs) = ArkhamWoodsCliffside <$> runMessage msg attrs
