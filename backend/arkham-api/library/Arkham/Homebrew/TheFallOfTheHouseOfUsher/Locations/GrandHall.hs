module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.GrandHall (grandHall) where

import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.Location.Import.Lifted

newtype GrandHall = GrandHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandHall :: LocationCard GrandHall
grandHall = location GrandHall Cards.grandHall 2 (Static 0)

instance HasAbilities GrandHall where
  getAbilities (GrandHall a) =
    extendRevealed
      a
      [ locationResignAction a
      , scenarioI18n $ collapseForced "grandHall.collapse" a 1
      ]

instance RunMessage GrandHall where
  runMessage msg l@(GrandHall attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      flipToRuined attrs
      pure l
    _ -> GrandHall <$> liftRunMessage msg attrs
