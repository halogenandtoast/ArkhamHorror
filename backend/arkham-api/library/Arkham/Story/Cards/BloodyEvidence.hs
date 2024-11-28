module Arkham.Story.Cards.BloodyEvidence
  ( BloodyEvidence(..)
  , bloodyEvidence
  ) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype BloodyEvidence = BloodyEvidence StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodyEvidence :: StoryCard BloodyEvidence
bloodyEvidence = story BloodyEvidence Cards.bloodyEvidence

instance RunMessage BloodyEvidence where
  runMessage msg s@(BloodyEvidence attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> BloodyEvidence <$> liftRunMessage msg attrs
