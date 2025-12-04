module Arkham.Story.Cards.ExhumeTheBones (exhumeTheBones) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype ExhumeTheBones = ExhumeTheBones StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhumeTheBones :: StoryCard ExhumeTheBones
exhumeTheBones = story ExhumeTheBones Cards.exhumeTheBones

instance RunMessage ExhumeTheBones where
  runMessage msg s@(ExhumeTheBones attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> ExhumeTheBones <$> liftRunMessage msg attrs
