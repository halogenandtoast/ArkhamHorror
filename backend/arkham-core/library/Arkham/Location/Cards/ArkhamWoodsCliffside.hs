module Arkham.Location.Cards.ArkhamWoodsCliffside where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ArkhamWoodsCliffside = ArkhamWoodsCliffside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

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
