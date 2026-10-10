module Arkham.Homebrew.AgesUnwound.Locations.AnEarthLongDead_074 (anEarthLongDead_074) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AnEarthLongDead_074 = AnEarthLongDead_074 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /An Earth Long Dead/, the printing that suffocates you instead.
anEarthLongDead_074 :: LocationCard AnEarthLongDead_074
anEarthLongDead_074 =
  locationWith AnEarthLongDead_074 Cards.anEarthLongDead_074 6 (Static 0)
    $ connectsToL
    .~ ringConnections

{- | "Forced - After you enter this location: Take 1 damage and 1 horror."
See 'AnEarthLongDead_073' for why this is ability 2 rather than
'endOfTurnAbility'.
-}
instance HasAbilities AnEarthLongDead_074 where
  getAbilities (AnEarthLongDead_074 a) =
    extendRevealed1 a $ mkAbility a 2 $ forced $ Enters #after You (be a)

instance RunMessage AnEarthLongDead_074 where
  runMessage msg l@(AnEarthLongDead_074 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignDamageAndHorror iid (attrs.ability 2) 1 1
      pure l
    _ -> AnEarthLongDead_074 <$> liftRunMessage msg attrs
