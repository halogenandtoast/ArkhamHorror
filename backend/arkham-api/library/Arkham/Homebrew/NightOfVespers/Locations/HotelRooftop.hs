module Arkham.Homebrew.NightOfVespers.Locations.HotelRooftop (hotelRooftop) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.NightOfVespers.Scenarios.TheEndOfSeasons.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log

newtype HotelRooftop = HotelRooftop LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotelRooftop :: LocationCard HotelRooftop
hotelRooftop = symbolLabel $ location HotelRooftop Cards.hotelRooftop 4 (PerPlayer 2)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities HotelRooftop where
  getAbilities (HotelRooftop a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : ( guard a.revealed
            *> [skillTestAbility $ restricted a 2 (Here <> NoCluesOnThis) actionAbility]
        )

instance RunMessage HotelRooftop where
  runMessage msg l@(HotelRooftop attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.grandBallroom
          ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 3)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      remember forcedOpenTheRooftopSkylight
      pure l
    _ -> HotelRooftop <$> liftRunMessage msg attrs
