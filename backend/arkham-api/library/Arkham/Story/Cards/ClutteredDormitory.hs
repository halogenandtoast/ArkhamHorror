module Arkham.Story.Cards.ClutteredDormitory (clutteredDormitory) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype ClutteredDormitory = ClutteredDormitory StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clutteredDormitory :: StoryCard ClutteredDormitory
clutteredDormitory = story ClutteredDormitory Cards.clutteredDormitory

instance RunMessage ClutteredDormitory where
  runMessage msg s@(ClutteredDormitory attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> ClutteredDormitory <$> liftRunMessage msg attrs
