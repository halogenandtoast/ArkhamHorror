module Arkham.Story.Cards.Airfield (airfield) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype Airfield = Airfield StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

airfield :: StoryCard Airfield
airfield = story Airfield Cards.airfield

instance RunMessage Airfield where
  runMessage msg s@(Airfield attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> Airfield <$> liftRunMessage msg attrs
