module Arkham.Story.Cards.OffTheGalley (offTheGalley) where

import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Movement
import Arkham.Projection
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype OffTheGalley = OffTheGalley StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

offTheGalley :: StoryCard OffTheGalley
offTheGalley = story OffTheGalley Cards.offTheGalley

instance RunMessage OffTheGalley where
  runMessage msg s@(OffTheGalley attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      moonBeastGalley <- selectJust $ locationIs Locations.moonBeastGalley
      moonForest <- selectJust $ locationIs Locations.moonForest
      clues <- field LocationClues moonBeastGalley
      investigators <- select $ investigatorAt moonBeastGalley
      if clues == 0
        then do
          selectEach (enemyAt moonBeastGalley) (toDiscard attrs)
          for_ investigators \iid ->
            moveToEdit attrs iid moonForest \m -> m {movePayAdditionalCosts = False, moveCancelable = False}
        else do
          raiseAlarmLevel attrs investigators
          for_ investigators \iid ->
            moveToEdit attrs iid moonForest \m -> m {movePayAdditionalCosts = False, moveCancelable = False}
          doStep 2 msg

      removeFromGame moonBeastGalley
      pure s
    DoStep 2 (ResolveThisStory _ (is attrs -> True)) -> do
      moonBeastGalley <- selectJust $ locationIs Locations.moonBeastGalley
      moonForest <- selectJust $ locationIs Locations.moonForest
      selectEach (enemyAt moonBeastGalley) \eid -> enemyMoveTo attrs eid moonForest
      pure s
    _ -> OffTheGalley <$> liftRunMessage msg attrs
