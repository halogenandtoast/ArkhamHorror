module Arkham.Story.Cards.StepsOfThePalace (stepsOfThePalace) where

import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype StepsOfThePalace = StepsOfThePalace StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stepsOfThePalace :: StoryCard StepsOfThePalace
stepsOfThePalace = story StepsOfThePalace Cards.stepsOfThePalace

instance RunMessage StepsOfThePalace where
  runMessage msg s@(StepsOfThePalace attrs) = runQueueT $ case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      n <- perPlayer 1
      storyEnemyDamage iid n hastur
      exhaustThis hastur
      selectEach (investigatorEngagedWith hastur) (`disengageEnemy` hastur)
      pure s
    _ -> StepsOfThePalace <$> liftRunMessage msg attrs
