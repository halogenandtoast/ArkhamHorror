module Arkham.Story.Cards.ChildrenOfBlood.BloodToken (bloodToken) where

import Arkham.Story.CardDefs.ChildrenOfBlood qualified as Cards
import Arkham.Story.Import.Lifted

newtype BloodToken = BloodToken StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodToken :: StoryCard BloodToken
bloodToken = story BloodToken Cards.bloodToken

instance RunMessage BloodToken where
  runMessage msg s@(BloodToken attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> BloodToken <$> liftRunMessage msg attrs
