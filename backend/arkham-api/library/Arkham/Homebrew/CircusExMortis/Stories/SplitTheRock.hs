module Arkham.Homebrew.CircusExMortis.Stories.SplitTheRock (splitTheRock) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Story.Import.Lifted

{- | Destiny "rock" (:204). "Flip this card over and put it into play" -- the
back is the Canyon Entrance location (:204b). 'otherSideIs' makes that def
single-sided, so it enters play already revealed: the location face IS the revealed face
of the physical card, and no 'RevealLocation' ever fires for it.
-}
newtype SplitTheRock = SplitTheRock StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

splitTheRock :: StoryCard SplitTheRock
splitTheRock = story SplitTheRock Cards.splitTheRock

instance RunMessage SplitTheRock where
  runMessage msg s@(SplitTheRock attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      placeLocation_ Locations.canyonEntrance
      pure s
    _ -> SplitTheRock <$> liftRunMessage msg attrs
