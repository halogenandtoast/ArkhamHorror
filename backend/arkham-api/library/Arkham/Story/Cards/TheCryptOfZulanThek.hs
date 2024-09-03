module Arkham.Story.Cards.TheCryptOfZulanThek (TheCryptOfZulanThek (..), theCryptOfZulanThek) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TheCryptOfZulanThek = TheCryptOfZulanThek StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCryptOfZulanThek :: StoryCard TheCryptOfZulanThek
theCryptOfZulanThek = story TheCryptOfZulanThek Cards.theCryptOfZulanThek

instance RunMessage TheCryptOfZulanThek where
  runMessage msg s@(TheCryptOfZulanThek attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      mHordeOfNight <- selectOne $ enemyIs Enemies.hordeOfNight <> IsHost
      pushAll $ ScenarioCountIncrementBy SignOfTheGods 1 : map addToVictory (maybeToList mHordeOfNight)
      pure s
    _ -> TheCryptOfZulanThek <$> runMessage msg attrs
