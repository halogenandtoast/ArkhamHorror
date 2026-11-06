module Arkham.Homebrew.NightOfVespers.Locations.GalerieLavandiere_015 (galerieLavandiere_015) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GalerieLavandiere_015 = GalerieLavandiere_015 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

galerieLavandiere_015 :: LocationCard GalerieLavandiere_015
galerieLavandiere_015 = symbolLabel $ location GalerieLavandiere_015 Cards.galerieLavandiere_015 3 (PerPlayer 1)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities GalerieLavandiere_015 where
  getAbilities (GalerieLavandiere_015 a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : (guard a.revealed *> [mkAbility a 2 $ forced $ RevealLocation #when You (be a)])

instance RunMessage GalerieLavandiere_015 where
  runMessage msg l@(GalerieLavandiere_015 attrs) = runQueueT $ case msg of
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
      createEnemyAtLocationMatching_ card (UnrevealedLocation <> connectedTo (be attrs))
      pure l
    _ -> GalerieLavandiere_015 <$> liftRunMessage msg attrs
