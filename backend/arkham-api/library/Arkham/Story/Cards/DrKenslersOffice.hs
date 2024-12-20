module Arkham.Story.Cards.DrKenslersOffice (drKenslersOffice) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DrKenslersOffice = DrKenslersOffice StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drKenslersOffice :: StoryCard DrKenslersOffice
drKenslersOffice = story DrKenslersOffice Cards.drKenslersOffice

instance RunMessage DrKenslersOffice where
  runMessage msg s@(DrKenslersOffice attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> DrKenslersOffice <$> liftRunMessage msg attrs
