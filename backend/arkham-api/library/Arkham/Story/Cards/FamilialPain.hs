module Arkham.Story.Cards.FamilialPain (familialPain) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype FamilialPain = FamilialPain StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

familialPain :: StoryCard FamilialPain
familialPain = story FamilialPain Cards.familialPain

instance RunMessage FamilialPain where
  runMessage msg s@(FamilialPain attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> FamilialPain <$> liftRunMessage msg attrs
