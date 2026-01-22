module Arkham.Story.Cards.NightThree (nightThree) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype NightThree = NightThree StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightThree :: StoryCard NightThree
nightThree = story NightThree Cards.nightThree

instance RunMessage NightThree where
  runMessage msg s@(NightThree attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> NightThree <$> liftRunMessage msg attrs
