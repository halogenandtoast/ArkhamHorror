module Arkham.Homebrew.NightOfVespers.Locations.Conservatory (conservatory) where

import Arkham.Ability
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.NightOfVespers.Scenarios.TheEndOfSeasons.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log

newtype Conservatory = Conservatory LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

conservatory :: LocationCard Conservatory
conservatory = symbolLabel $ location Conservatory Cards.conservatory 3 (PerPlayer 2)

-- Printed on the unrevealed side but offered on both, because the window resolves
-- after this location is revealed. It rides the after-move window so that the
-- act's after-enter removal of unrevealed locations resolves first.
instance HasAbilities Conservatory where
  getAbilities (Conservatory a) =
    extend a
      $ mkAbility a 1 (forced $ Moves #after You AnySource Anywhere (be a))
      : ( guard a.revealed
            *> [skillTestAbility $ restricted a 2 (Here <> NoCluesOnThis) actionAbility]
        )

instance RunMessage Conservatory where
  runMessage msg l@(Conservatory attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocationsMatching_
        $ mapOneOf
          cardIs
          [ Cards.grandBallroom
          ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #combat (Fixed 3)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      remember threwAChairThroughTheConservatoryWindow
      pure l
    _ -> Conservatory <$> liftRunMessage msg attrs
