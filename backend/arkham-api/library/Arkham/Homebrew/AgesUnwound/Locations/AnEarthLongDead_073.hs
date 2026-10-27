module Arkham.Homebrew.AgesUnwound.Locations.AnEarthLongDead_073 (anEarthLongDead_073) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AnEarthLongDead_073 = AnEarthLongDead_073 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anEarthLongDead_073 :: LocationCard AnEarthLongDead_073
anEarthLongDead_073 =
  locationWith AnEarthLongDead_073 Cards.anEarthLongDead_073 6 (Static 0)
    $ connectsToL
    .~ ringConnections

instance HasAbilities AnEarthLongDead_073 where
  getAbilities (AnEarthLongDead_073 a) =
    extendRevealed1 a
      $ mkAbility a 2
      $ forced
      $ Enters #after (You <> InvestigatorWithAnyActionsRemaining) (be a)

instance RunMessage AnEarthLongDead_073 where
  runMessage msg l@(AnEarthLongDead_073 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      loseActions iid (attrs.ability 2) 1
      pure l
    _ -> AnEarthLongDead_073 <$> liftRunMessage msg attrs
