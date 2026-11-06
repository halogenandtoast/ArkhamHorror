-- The unrevealed Forced ability is implemented; revealed abilities remain unimplemented.
module Arkham.Homebrew.NightOfVespers.Locations.MezzanineElevators_023 (mezzanineElevators_023) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MezzanineElevators_023 = MezzanineElevators_023 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mezzanineElevators_023 :: LocationCard MezzanineElevators_023
mezzanineElevators_023 = symbolLabel $ location MezzanineElevators_023 Cards.mezzanineElevators_023 3 (PerPlayer 1)

-- The entry window resolves after this location is revealed, so the Forced
-- ability printed on its unrevealed side must be available on both sides.
instance HasAbilities MezzanineElevators_023 where
  getAbilities (MezzanineElevators_023 attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ Enters #after You (be attrs)

instance RunMessage MezzanineElevators_023 where
  runMessage msg l@(MezzanineElevators_023 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.luxurySuites_021
          , Cards.luxurySuites_022
          , Cards.presidentialSuite_027
          , Cards.presidentialSuite_028
          ]
      pure l
    _ -> MezzanineElevators_023 <$> liftRunMessage msg attrs
