module Arkham.Story.Cards.HastursEnd (hastursEnd) where

import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype HastursEnd = HastursEnd StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hastursEnd :: StoryCard HastursEnd
hastursEnd = story HastursEnd Cards.hastursEnd

instance RunMessage HastursEnd where
  runMessage msg s@(HastursEnd attrs) = runQueueT $ case msg of
    ResolveStory _ _ (is attrs -> True) -> do
      remember KnowTheSecret
      selectEach (EnemyWithTitle "Hastur") (checkDefeated attrs)
      pure s
    _ -> HastursEnd <$> liftRunMessage msg attrs
