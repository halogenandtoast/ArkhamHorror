module Arkham.Homebrew.DarkMatter.Stories.FinalDestination (finalDestination) where

import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (addMemories)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype FinalDestination = FinalDestination StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finalDestination :: StoryCard FinalDestination
finalDestination = story FinalDestination Cards.finalDestination

instance RunMessage FinalDestination where
  runMessage msg s@(FinalDestination attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      colocated <- select $ colocatedWith iid
      for_ colocated (`addMemories` 1)
      withI18n $ chooseAmount' iid "resources" "$resources" 0 3 attrs
      addToVictory iid attrs
      pure s
    ResolveAmounts iid (getChoiceAmount "$resources" -> n) (isTarget attrs -> True) -> do
      when (n > 0) $ gainResources iid attrs n
      pure s
    _ -> FinalDestination <$> liftRunMessage msg attrs
