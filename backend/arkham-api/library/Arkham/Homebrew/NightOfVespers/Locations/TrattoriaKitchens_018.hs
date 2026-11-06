-- The unrevealed Forced ability is implemented; revealed abilities remain unimplemented.
module Arkham.Homebrew.NightOfVespers.Locations.TrattoriaKitchens_018 (trattoriaKitchens_018) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TrattoriaKitchens_018 = TrattoriaKitchens_018 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trattoriaKitchens_018 :: LocationCard TrattoriaKitchens_018
trattoriaKitchens_018 = symbolLabel $ location TrattoriaKitchens_018 Cards.trattoriaKitchens_018 1 (PerPlayer 2)

-- The entry window resolves after this location is revealed, so the Forced
-- ability printed on its unrevealed side must be available on both sides.
instance HasAbilities TrattoriaKitchens_018 where
  getAbilities (TrattoriaKitchens_018 attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ Enters #after You (be attrs)

instance RunMessage TrattoriaKitchens_018 where
  runMessage msg l@(TrattoriaKitchens_018 attrs) = runQueueT $ case msg of
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
    _ -> TrattoriaKitchens_018 <$> liftRunMessage msg attrs
