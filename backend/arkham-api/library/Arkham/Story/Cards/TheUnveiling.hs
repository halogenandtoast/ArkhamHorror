module Arkham.Story.Cards.TheUnveiling (theUnveiling) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Token

newtype TheUnveiling = TheUnveiling StoryAttrs
  deriving anyclass (IsStory, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theUnveiling :: StoryCard TheUnveiling
theUnveiling = story TheUnveiling Cards.theUnveiling

instance HasModifiersFor TheUnveiling where
  getModifiersFor (TheUnveiling a) = do
    cultistClues <- selectSum EnemyClues (InPlayEnemy #cultist)
    n <- perPlayer 4
    when (cultistClues + a.token Clue >= n) do
      modifySelf a [ScenarioModifier "cultHasEnoughClues"]

instance RunMessage TheUnveiling where
  runMessage msg s@(TheUnveiling attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> TheUnveiling <$> liftRunMessage msg attrs
