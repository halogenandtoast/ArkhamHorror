module Arkham.Location.Cards.GardensOfLuxembourg (gardensOfLuxembourg) where

import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype GardensOfLuxembourg = GardensOfLuxembourg LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

gardensOfLuxembourg :: LocationCard GardensOfLuxembourg
gardensOfLuxembourg = location GardensOfLuxembourg Cards.gardensOfLuxembourg 3 (PerPlayer 1)

instance HasModifiersFor GardensOfLuxembourg where
  getModifiersFor (GardensOfLuxembourg a) = whenRevealed a do
    modifySelect
      a
      (LocationWithEnemy $ MovingEnemy <> EnemyWithTrait Byakhee)
      [ConnectedToWhen Anywhere (be a)]
