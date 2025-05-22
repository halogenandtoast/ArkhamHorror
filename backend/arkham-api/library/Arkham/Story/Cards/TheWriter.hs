module Arkham.Story.Cards.TheWriter (theWriter) where

import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheWriter = TheWriter StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWriter :: StoryCard TheWriter
theWriter = story TheWriter Cards.theWriter

instance RunMessage TheWriter where
  runMessage msg s@(TheWriter attrs) = runQueueT $ case msg of
    ResolveStory _ _ (is attrs -> True) -> do
      remember KnowTheSecret
      selectEach (EnemyWithTitle "Hastur") (checkDefeated attrs)
      pure s
    _ -> TheWriter <$> liftRunMessage msg attrs
