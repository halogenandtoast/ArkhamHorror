module Arkham.Homebrew.NightOfVespers.Locations.TrattoriaKitchens_018 (trattoriaKitchens_018) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Trait (Trait (Supply))

newtype TrattoriaKitchens_018 = TrattoriaKitchens_018 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trattoriaKitchens_018 :: LocationCard TrattoriaKitchens_018
trattoriaKitchens_018 = symbolLabel $ location TrattoriaKitchens_018 Cards.trattoriaKitchens_018 1 (PerPlayer 2)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities TrattoriaKitchens_018 where
  getAbilities (TrattoriaKitchens_018 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : ( guard a.revealed
            *> [ mkAbility a 2 $ forced $ RevealLocation #when You (be a)
               , playerLimit PerGame $ restricted a 3 Here actionAbility
               ]
        )

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
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignDamage iid (attrs.ability 2) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      search iid (attrs.ability 3) iid [fromTopOfDeck 9] (basic $ withTrait Supply) (AddFoundToHand iid 1)
      pure l
    _ -> TrattoriaKitchens_018 <$> liftRunMessage msg attrs
