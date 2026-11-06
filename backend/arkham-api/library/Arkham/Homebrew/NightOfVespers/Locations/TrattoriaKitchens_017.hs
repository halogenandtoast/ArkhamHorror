-- The unrevealed Forced ability is implemented; revealed abilities remain unimplemented.
module Arkham.Homebrew.NightOfVespers.Locations.TrattoriaKitchens_017 (trattoriaKitchens_017) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TrattoriaKitchens_017 = TrattoriaKitchens_017 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trattoriaKitchens_017 :: LocationCard TrattoriaKitchens_017
trattoriaKitchens_017 = symbolLabel $ location TrattoriaKitchens_017 Cards.trattoriaKitchens_017 1 (PerPlayer 2)

-- The entry window resolves after this location is revealed, so the Forced
-- ability printed on its unrevealed side must be available on both sides.
instance HasAbilities TrattoriaKitchens_017 where
  getAbilities (TrattoriaKitchens_017 attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ Enters #after You (be attrs)

instance RunMessage TrattoriaKitchens_017 where
  runMessage msg l@(TrattoriaKitchens_017 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.galerieLavandiere_015
          , Cards.galerieLavandiere_016
          , Cards.hotelWineCellar_029
          , Cards.hotelWineCellar_030
          ]
      pure l
    _ -> TrattoriaKitchens_017 <$> liftRunMessage msg attrs
