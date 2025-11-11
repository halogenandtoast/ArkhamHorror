module Arkham.Story.Cards.TheStakeout (theStakeout) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheStakeout = TheStakeout StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStakeout :: StoryCard TheStakeout
theStakeout = story TheStakeout Cards.theStakeout

instance RunMessage TheStakeout where
  runMessage msg s@(TheStakeout attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> TheStakeout <$> liftRunMessage msg attrs
