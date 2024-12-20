module Arkham.Story.Cards.Infirmary (infirmary) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype Infirmary = Infirmary StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infirmary :: StoryCard Infirmary
infirmary = story Infirmary Cards.infirmary

instance RunMessage Infirmary where
  runMessage msg s@(Infirmary attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> Infirmary <$> liftRunMessage msg attrs
