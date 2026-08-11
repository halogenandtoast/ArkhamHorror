module Arkham.Homebrew.DarkMatter.Stories.StrangeIsTheNight (strangeIsTheNight) where

import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (addMemories)
import Arkham.Story.Import.Lifted

newtype StrangeIsTheNight = StrangeIsTheNight StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeIsTheNight :: StoryCard StrangeIsTheNight
strangeIsTheNight = story StrangeIsTheNight Cards.strangeIsTheNight

-- "Add 2 tally marks next to your 'Memories'. Add this card to the victory display."
instance RunMessage StrangeIsTheNight where
  runMessage msg s@(StrangeIsTheNight attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      addMemories iid 2
      addToVictory iid attrs
      pure s
    _ -> StrangeIsTheNight <$> liftRunMessage msg attrs
