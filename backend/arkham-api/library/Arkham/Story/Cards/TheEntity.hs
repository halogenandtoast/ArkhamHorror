module Arkham.Story.Cards.TheEntity (theEntity) where

import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheEntity = TheEntity StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEntity :: StoryCard TheEntity
theEntity = story TheEntity Cards.theEntity

instance RunMessage TheEntity where
  runMessage msg s@(TheEntity attrs) = runQueueT $ case msg of
    ResolveStory _ _ (is attrs -> True) -> do
      remember KnowTheSecret
      selectEach (EnemyWithTitle "Hastur") (checkDefeated attrs)
      pure s
    _ -> TheEntity <$> liftRunMessage msg attrs
