module Arkham.Homebrew.NightOfVespers.Locations.GalerieLavandiere_016 (galerieLavandiere_016) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GalerieLavandiere_016 = GalerieLavandiere_016 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

galerieLavandiere_016 :: LocationCard GalerieLavandiere_016
galerieLavandiere_016 = symbolLabel $ location GalerieLavandiere_016 Cards.galerieLavandiere_016 3 (PerPlayer 1)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities GalerieLavandiere_016 where
  getAbilities (GalerieLavandiere_016 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : (guard a.revealed *> [mkAbility a 2 $ forced $ RevealLocation #when You (be a)])

instance RunMessage GalerieLavandiere_016 where
  runMessage msg l@(GalerieLavandiere_016 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.hotelLobby_019
          , Cards.hotelLobby_020
          , Cards.luxurySuites_021
          , Cards.luxurySuites_022
          ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discardUntilFirst iid (attrs.ability 2) Deck.EncounterDeck #enemy
      pure l
    RequestedEncounterCard (isAbilitySource attrs 2 -> True) _ (Just card) -> do
      createEnemyAtLocationMatching_
        card
        (mapOneOf LocationWithTitle ["Trattoria Kitchens", "Bar L'Ammaliatrice"])
      pure l
    _ -> GalerieLavandiere_016 <$> liftRunMessage msg attrs
