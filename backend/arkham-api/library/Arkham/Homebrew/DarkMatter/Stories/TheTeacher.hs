module Arkham.Homebrew.DarkMatter.Stories.TheTeacher (theTeacher) where

import Arkham.Helpers.Scenario (inVictoryDisplay)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (returnToScanningDeck)
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype TheTeacher = TheTeacher StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTeacher :: StoryCard TheTeacher
theTeacher = story TheTeacher Cards.theTeacher

{- | "If A Hiding Place is in the victory display: Add this card to the victory
display. / Otherwise: Heal 1 horror and shuffle this card back into the scanning
deck."
-}
instance RunMessage TheTeacher where
  runMessage msg s@(TheTeacher attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      remembered <- inVictoryDisplay (cardIs Locations.aHidingPlace)
      if remembered
        then addToVictory iid attrs
        else do
          healHorror iid attrs 1
          returnToScanningDeck attrs
      pure s
    _ -> TheTeacher <$> liftRunMessage msg attrs
