module Arkham.Story.Cards.SaveTheCivilians (saveTheCivilians) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype SaveTheCivilians = SaveTheCivilians StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

saveTheCivilians :: StoryCard SaveTheCivilians
saveTheCivilians = story SaveTheCivilians Cards.saveTheCivilians

instance RunMessage SaveTheCivilians where
  runMessage msg s@(SaveTheCivilians attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> SaveTheCivilians <$> liftRunMessage msg attrs
