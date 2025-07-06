module Arkham.Story.Cards.OffTheGalley (offTheGalley) where

import Arkham.Helpers.Query (allInvestigators)
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
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      moonBeastGalley <- selectJust $ locationIs Locations.moonBeastGalley
      moonForest <- selectJust $ locationIs Locations.moonForest
      clues <- field LocationClues moonBeastGalley
      investigators <- select $ investigatorAt moonBeastGalley
      enemies <- select $ enemyAt moonBeastGalley <> oneOf [UnengagedEnemy, MassiveEnemy]
      if clues == 0
        then do
          for_ investigators $ \iid ->
            moveToEdit attrs iid moonForest \m -> m {movePayAdditionalCosts = False, moveCancelable = False}
        else do
          raiseAlarmLevel attrs =<< allInvestigators
          eachInvestigator $ \iid ->
            moveToEdit attrs iid moonForest \m -> m {movePayAdditionalCosts = False, moveCancelable = False}
          for_ enemies $ \eid -> enemyMoveTo attrs eid moonForest

      removeFromGame moonBeastGalley
      pure s
    _ -> OffTheGalley <$> liftRunMessage msg attrs
