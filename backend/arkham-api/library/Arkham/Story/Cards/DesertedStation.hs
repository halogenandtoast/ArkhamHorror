module Arkham.Story.Cards.DesertedStation (desertedStation) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DesertedStation = DesertedStation StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desertedStation :: StoryCard DesertedStation
desertedStation = story DesertedStation Cards.desertedStation

instance RunMessage DesertedStation where
  runMessage msg s@(DesertedStation attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> DesertedStation <$> liftRunMessage msg attrs
