module Arkham.Story.Cards.TheBalefulStar (theBalefulStar) where

import Arkham.Helpers.Query (getJustLocationByName)
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Movement
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheBalefulStar = TheBalefulStar StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBalefulStar :: StoryCard TheBalefulStar
theBalefulStar = story TheBalefulStar Cards.theBalefulStar

instance RunMessage TheBalefulStar where
  runMessage msg s@(TheBalefulStar attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      cityWhichAppearsOnNoMap <- getJustLocationByName "City-Which-Appears-On-No-Map"
      templeOfUnattainableDesires <- getJustLocationByName "Temple of Unattainable Desires"
      push $ ScenarioCountIncrementBy SignOfTheGods 2
      selectEach (enemyAt cityWhichAppearsOnNoMap) (toDiscard attrs)
      selectEach (investigatorAt cityWhichAppearsOnNoMap) \iid -> do
        moveToEdit attrs iid templeOfUnattainableDesires uncancellableMove
      addToVictory cityWhichAppearsOnNoMap
      pure s
    _ -> TheBalefulStar <$> liftRunMessage msg attrs
