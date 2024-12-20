module Arkham.Story.Cards.ElderChamber (elderChamber) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype ElderChamber = ElderChamber StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elderChamber :: StoryCard ElderChamber
elderChamber = story ElderChamber Cards.elderChamber

instance RunMessage ElderChamber where
  runMessage msg s@(ElderChamber attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> ElderChamber <$> liftRunMessage msg attrs
