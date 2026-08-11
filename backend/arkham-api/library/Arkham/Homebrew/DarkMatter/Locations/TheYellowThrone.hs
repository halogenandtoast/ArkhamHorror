module Arkham.Homebrew.DarkMatter.Locations.TheYellowThrone (theYellowThrone) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.DarkMatter.Helpers (flipToOtherSide)
import Arkham.Homebrew.DarkMatter.Traits (pattern Carcosa)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TheYellowThrone = TheYellowThrone LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The [[Carcosa]] face of Impassable Ravine.
theYellowThrone :: LocationCard TheYellowThrone
theYellowThrone =
  locationWith TheYellowThrone Cards.theYellowThrone 2 (PerPlayer 1) (canBeFlippedL .~ True)

{- | "{fast} If there are 6 [[Carcosa]] locations in play and each undefeated
investigator is at this location: Read the set aside "Lost Expedition" story
card. (Max once per game.)"
-}
instance HasAbilities TheYellowThrone where
  getAbilities (TheYellowThrone a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted
        a
        1
        ( Here
            <> LocationCount 6 (LocationWithTrait Carcosa)
            <> not_ (exists $ UneliminatedInvestigator <> not_ (investigatorAt a.id))
        )
      $ freeReaction AnyWindow

instance RunMessage TheYellowThrone where
  runMessage msg l@(TheYellowThrone attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readStory iid attrs.id Stories.lostExpedition
      pure l
    Flip _ _ (isTarget attrs -> True) -> do
      flipToOtherSide attrs
      pure l
    _ -> TheYellowThrone <$> liftRunMessage msg attrs
