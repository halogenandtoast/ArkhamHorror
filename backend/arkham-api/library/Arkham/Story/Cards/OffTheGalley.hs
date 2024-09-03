module Arkham.Story.Cards.OffTheGalley (OffTheGalley (..), offTheGalley) where

import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted hiding (story)
import Arkham.Movement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

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
          for_ investigators $ \iid -> push $ Move $ move attrs iid moonForest
        else do
          eachInvestigator (raiseAlarmLevel attrs)
          eachInvestigator $ \iid -> push $ Move $ move attrs iid moonForest
          for_ enemies $ \eid -> push $ Move $ move attrs eid moonForest

      push $ RemoveFromGame (toTarget moonBeastGalley)
      pure s
    _ -> OffTheGalley <$> liftRunMessage msg attrs
