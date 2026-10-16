{- | Dining Room -- Bedrooms' sibling, paid for in horror instead of cards.

'InvestigatorWithHorror' is the engine's reading of "horror on your investigator
card" (it folds in the horror Foolishness and Rational Thought hold on your
behalf), and the window already scopes the discovery to this location, so the
ability needs no @Here@.
-}
module Arkham.Homebrew.AndThenThereWereNone.Locations.DiningRoom (diningRoom) where

import Arkham.Ability
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DiningRoom = DiningRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diningRoom :: LocationCard DiningRoom
diningRoom = location DiningRoom Cards.diningRoom 2 (PerPlayer 2)

instance HasAbilities DiningRoom where
  getAbilities (DiningRoom a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "diningRoom.clue"
      -- "(Limit once per round.)"
      $ playerLimit PerRound
      -- "if there is 4 or more horror on your investigator card"
      $ restricted a 1 (youExist $ InvestigatorWithHorror $ atLeast 4)
      $ freeReaction (DiscoverClues #after You (be a) (atLeast 1))

instance RunMessage DiningRoom where
  runMessage msg l@(DiningRoom attrs) = runQueueT $ case msg of
    -- "Discover 1 additional clue."
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAt NotInvestigate iid (attrs.ability 1) 1 attrs
      pure l
    _ -> DiningRoom <$> liftRunMessage msg attrs
