module Arkham.Story.Cards.RealityAcid (realityAcid) where

-- Placeholder so appears in cards UI

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype RealityAcid = RealityAcid StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realityAcid :: StoryCard RealityAcid
realityAcid = story RealityAcid Cards.realityAcid

instance RunMessage RealityAcid where
  runMessage msg s@(RealityAcid attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> RealityAcid <$> liftRunMessage msg attrs
