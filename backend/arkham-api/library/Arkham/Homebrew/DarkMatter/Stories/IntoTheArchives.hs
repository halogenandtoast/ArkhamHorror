module Arkham.Homebrew.DarkMatter.Stories.IntoTheArchives (intoTheArchives) where

import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (addMemories)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype IntoTheArchives = IntoTheArchives StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheArchives :: StoryCard IntoTheArchives
intoTheArchives = story IntoTheArchives Cards.intoTheArchives

{- | "Each investigator at your location adds 1 tally mark next to their
'Memories'. You may heal up to 2 horror. Add this card to the victory display."
-}
instance RunMessage IntoTheArchives where
  runMessage msg s@(IntoTheArchives attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      colocated <- select $ colocatedWith iid
      for_ colocated (`addMemories` 1)
      withI18n $ chooseAmount' iid "horror" "$horror" 0 2 attrs
      addToVictory iid attrs
      pure s
    ResolveAmounts iid (getChoiceAmount "horror" -> n) (isTarget attrs -> True) -> do
      when (n > 0) $ healHorror iid attrs n
      pure s
    _ -> IntoTheArchives <$> liftRunMessage msg attrs
