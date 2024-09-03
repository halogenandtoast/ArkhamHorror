module Arkham.Story.Cards.TheBalefulStar (TheBalefulStar (..), theBalefulStar) where

import Arkham.Matcher
import Arkham.Movement
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TheBalefulStar = TheBalefulStar StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBalefulStar :: StoryCard TheBalefulStar
theBalefulStar = story TheBalefulStar Cards.theBalefulStar

instance RunMessage TheBalefulStar where
  runMessage msg s@(TheBalefulStar attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      cityWhichAppearsOnNoMap <- getJustLocationByName "City-Which-Appears-On-No-Map"
      templeOfUnattainableDesires <- getJustLocationByName "Temple of Unattainable Desires"
      enemies <- select $ enemyAt cityWhichAppearsOnNoMap
      investigators <- select $ investigatorAt cityWhichAppearsOnNoMap
      pushAll
        $ [ScenarioCountIncrementBy SignOfTheGods 2]
        <> map (toDiscard attrs) enemies
        <> [MoveTo $ uncancellableMove $ move attrs iid templeOfUnattainableDesires | iid <- investigators]
        <> [AddToVictory $ toTarget cityWhichAppearsOnNoMap]
      pure s
    _ -> TheBalefulStar <$> runMessage msg attrs
