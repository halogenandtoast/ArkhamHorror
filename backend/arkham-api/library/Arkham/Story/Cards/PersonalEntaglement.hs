module Arkham.Story.Cards.PersonalEntaglement (personalEntaglement) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype PersonalEntaglement = PersonalEntaglement StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

personalEntaglement :: StoryCard PersonalEntaglement
personalEntaglement = story PersonalEntaglement Cards.personalEntaglement

instance RunMessage PersonalEntaglement where
  runMessage msg s@(PersonalEntaglement attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> PersonalEntaglement <$> liftRunMessage msg attrs
