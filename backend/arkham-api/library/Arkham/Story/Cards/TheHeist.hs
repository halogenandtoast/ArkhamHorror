module Arkham.Story.Cards.TheHeist (theHeist) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheHeist = TheHeist StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHeist :: StoryCard TheHeist
theHeist = story TheHeist Cards.theHeist

instance RunMessage TheHeist where
  runMessage msg s@(TheHeist attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> TheHeist <$> liftRunMessage msg attrs
