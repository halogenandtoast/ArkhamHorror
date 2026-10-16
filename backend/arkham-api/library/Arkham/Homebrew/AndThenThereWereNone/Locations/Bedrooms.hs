{- | Bedrooms -- an extra clue for a full hand.

The window already says the clues came from Bedrooms, so the ability carries no
@Here@: a discovery made from somewhere else still triggers it, exactly as
printed.
-}
module Arkham.Homebrew.AndThenThereWereNone.Locations.Bedrooms (bedrooms) where

import Arkham.Ability
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Bedrooms = Bedrooms LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedrooms :: LocationCard Bedrooms
bedrooms = location Bedrooms Cards.bedrooms 4 (PerPlayer 3)

instance HasAbilities Bedrooms where
  getAbilities (Bedrooms a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "bedrooms.clue"
      -- "(Limit once per round.)"
      $ playerLimit PerRound
      -- "if there are 6 or more cards in your hand"
      $ restricted a 1 (youExist $ HandWith $ LengthIs $ atLeast 6)
      $ freeReaction (DiscoverClues #after You (be a) (atLeast 1))

instance RunMessage Bedrooms where
  runMessage msg l@(Bedrooms attrs) = runQueueT $ case msg of
    -- "Discover 1 additional clue."
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAt NotInvestigate iid (attrs.ability 1) 1 attrs
      pure l
    _ -> Bedrooms <$> liftRunMessage msg attrs
