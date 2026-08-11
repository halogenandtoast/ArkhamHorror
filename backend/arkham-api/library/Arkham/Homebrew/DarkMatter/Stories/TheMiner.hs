module Arkham.Homebrew.DarkMatter.Stories.TheMiner (theMiner) where

import Arkham.Helpers.Scenario (inVictoryDisplay)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (returnToScanningDeck)
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype TheMiner = TheMiner StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMiner :: StoryCard TheMiner
theMiner = story TheMiner Cards.theMiner

{- | "If Adrift in Space is in the victory display: Add this card to the victory
display. / Otherwise: Take 1 horror and shuffle this card back into the scanning
deck."
-}
instance RunMessage TheMiner where
  runMessage msg s@(TheMiner attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      rescued <- inVictoryDisplay (cardIs Locations.adriftInSpace)
      if rescued
        then addToVictory iid attrs
        else do
          assignHorror iid attrs 1
          returnToScanningDeck attrs
      pure s
    _ -> TheMiner <$> liftRunMessage msg attrs
