module Arkham.Story.Cards.RecoverTheSample (recoverTheSample) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype RecoverTheSample = RecoverTheSample StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recoverTheSample :: StoryCard RecoverTheSample
recoverTheSample = story RecoverTheSample Cards.recoverTheSample

instance RunMessage RecoverTheSample where
  runMessage msg (RecoverTheSample attrs) = RecoverTheSample <$> runMessage msg attrs
