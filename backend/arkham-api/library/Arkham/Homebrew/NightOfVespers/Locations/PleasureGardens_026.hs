-- The unrevealed Forced ability is implemented; revealed abilities remain unimplemented.
module Arkham.Homebrew.NightOfVespers.Locations.PleasureGardens_026 (pleasureGardens_026) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype PleasureGardens_026 = PleasureGardens_026 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pleasureGardens_026 :: LocationCard PleasureGardens_026
pleasureGardens_026 = symbolLabel $ location PleasureGardens_026 Cards.pleasureGardens_026 3 (PerPlayer 2)

-- The entry window resolves after this location is revealed, so the Forced
-- ability printed on its unrevealed side must be available on both sides.
instance HasAbilities PleasureGardens_026 where
  getAbilities (PleasureGardens_026 attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ Enters #after You (be attrs)

instance RunMessage PleasureGardens_026 where
  runMessage msg l@(PleasureGardens_026 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.hedgeMaze
          , Cards.hotelRooftop
          ]
      pure l
    _ -> PleasureGardens_026 <$> liftRunMessage msg attrs
