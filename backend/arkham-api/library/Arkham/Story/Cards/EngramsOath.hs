module Arkham.Story.Cards.EngramsOath (engramsOath) where

import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype EngramsOath = EngramsOath StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

engramsOath :: StoryCard EngramsOath
engramsOath = story EngramsOath Cards.engramsOath

instance RunMessage EngramsOath where
  runMessage msg s@(EngramsOath attrs) = runQueueT $ case msg of
    ResolveStory _ _ (is attrs -> True) -> do
      remember InterviewedConstance
      pure s
    _ -> EngramsOath <$> liftRunMessage msg attrs
