-- The unrevealed Forced ability is implemented; revealed abilities remain unimplemented.
module Arkham.Homebrew.NightOfVespers.Locations.PleasureGardens_025 (pleasureGardens_025) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype PleasureGardens_025 = PleasureGardens_025 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pleasureGardens_025 :: LocationCard PleasureGardens_025
pleasureGardens_025 = symbolLabel $ location PleasureGardens_025 Cards.pleasureGardens_025 3 (PerPlayer 2)

-- The entry window resolves after this location is revealed, so the Forced
-- ability printed on its unrevealed side must be available on both sides.
instance HasAbilities PleasureGardens_025 where
  getAbilities (PleasureGardens_025 attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ Enters #after You (be attrs)

instance RunMessage PleasureGardens_025 where
  runMessage msg l@(PleasureGardens_025 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.hedgeMaze
          , Cards.hotelRooftop
          ]
      pure l
    _ -> PleasureGardens_025 <$> liftRunMessage msg attrs
