module Arkham.Story.Cards.OttomanFront (ottomanFront) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype OttomanFront = OttomanFront StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ottomanFront :: StoryCard OttomanFront
ottomanFront = story OttomanFront Cards.ottomanFront

instance RunMessage OttomanFront where
  runMessage msg s@(OttomanFront attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> OttomanFront <$> liftRunMessage msg attrs
