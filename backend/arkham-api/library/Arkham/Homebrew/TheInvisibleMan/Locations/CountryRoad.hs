{- | Country Road -- the way out.

Resign is the only thing printed on it. The engine's location abilities do not
include resign, so the card adds it.
-}
module Arkham.Homebrew.TheInvisibleMan.Locations.CountryRoad (countryRoad) where

import Arkham.Ability
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Location.Import.Lifted

newtype CountryRoad = CountryRoad LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

countryRoad :: LocationCard CountryRoad
countryRoad = location CountryRoad Cards.countryRoad 2 (PerPlayer 1)

instance HasAbilities CountryRoad where
  getAbilities (CountryRoad a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "countryRoad.resign"
      $ locationResignAction a

instance RunMessage CountryRoad where
  runMessage msg (CountryRoad attrs) = CountryRoad <$> runMessage msg attrs
