module Arkham.Story.Cards.PlayfulShadows (playfulShadows) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype PlayfulShadows = PlayfulShadows StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

playfulShadows :: StoryCard PlayfulShadows
playfulShadows = story PlayfulShadows Cards.playfulShadows

instance RunMessage PlayfulShadows where
  runMessage msg s@(PlayfulShadows attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> PlayfulShadows <$> liftRunMessage msg attrs
