module Arkham.Story.Cards.ReturnToThePathToCarcosa.ReturnToDimCarcosa.TheDelusion (theDelusion) where

import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.CardDefs.ReturnToThePathToCarcosa.ReturnToDimCarcosa qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheDelusion = TheDelusion StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDelusion :: StoryCard TheDelusion
theDelusion = story TheDelusion Cards.theDelusion

instance RunMessage TheDelusion where
  runMessage msg s@(TheDelusion attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      remember KnowTheSecret
      selectEach (EnemyWithTitle "Hastur") (checkDefeated attrs)
      pure s
    _ -> TheDelusion <$> liftRunMessage msg attrs
