module Arkham.Homebrew.DarkMatter.Locations.GrandBallroom (grandBallroom) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.DarkMatter.Helpers (flipToOtherSide)
import Arkham.Homebrew.DarkMatter.Traits (pattern Carcosa)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GrandBallroom = GrandBallroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The [[Carcosa]] face of Cyclopean Caverns.
grandBallroom :: LocationCard GrandBallroom
grandBallroom = locationWith GrandBallroom Cards.grandBallroom 4 (PerPlayer 2) (canBeFlippedL .~ True)

{- | "{fast} If this location is the only [[Carcosa]] location in play: Read the
set aside \"Arrival of the King\" story card. (Max once per game.)"
-}
instance HasAbilities GrandBallroom where
  getAbilities (GrandBallroom a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 (Here <> not_ (exists $ LocationWithTrait Carcosa <> not_ (be a)))
      $ freeReaction AnyWindow

instance RunMessage GrandBallroom where
  runMessage msg l@(GrandBallroom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readStory iid attrs.id Stories.arrivalOfTheKing
      pure l
    Flip _ _ (isTarget attrs -> True) -> do
      flipToOtherSide attrs
      pure l
    _ -> GrandBallroom <$> liftRunMessage msg attrs
