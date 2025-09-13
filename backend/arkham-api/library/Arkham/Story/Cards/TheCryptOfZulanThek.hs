module Arkham.Story.Cards.TheCryptOfZulanThek (theCryptOfZulanThek) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheCryptOfZulanThek = TheCryptOfZulanThek StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCryptOfZulanThek :: StoryCard TheCryptOfZulanThek
theCryptOfZulanThek = story TheCryptOfZulanThek Cards.theCryptOfZulanThek

instance RunMessage TheCryptOfZulanThek where
  runMessage msg s@(TheCryptOfZulanThek attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      push $ ScenarioCountIncrementBy SignOfTheGods 1
      mHordeOfNight <- selectOne $ enemyIs Enemies.hordeOfNight <> IsHost
      for_ mHordeOfNight addToVictory
      pure s
    _ -> TheCryptOfZulanThek <$> liftRunMessage msg attrs
