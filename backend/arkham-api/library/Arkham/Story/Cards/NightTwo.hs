module Arkham.Story.Cards.NightTwo (nightTwo) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype NightTwo = NightTwo StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightTwo :: StoryCard NightTwo
nightTwo = story NightTwo Cards.nightTwo

instance RunMessage NightTwo where
  runMessage msg s@(NightTwo attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> NightTwo <$> liftRunMessage msg attrs
