module Arkham.Story.Cards.TheBlackStone (theBlackStone) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheBlackStone = TheBlackStone StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackStone :: StoryCard TheBlackStone
theBlackStone = story TheBlackStone Cards.theBlackStone

instance RunMessage TheBlackStone where
  runMessage msg s@(TheBlackStone attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> TheBlackStone <$> liftRunMessage msg attrs
