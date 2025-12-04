module Arkham.Story.Cards.ALostMemento (aLostMemento) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype ALostMemento = ALostMemento StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aLostMemento :: StoryCard ALostMemento
aLostMemento = story ALostMemento Cards.aLostMemento

instance RunMessage ALostMemento where
  runMessage msg s@(ALostMemento attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> ALostMemento <$> liftRunMessage msg attrs
