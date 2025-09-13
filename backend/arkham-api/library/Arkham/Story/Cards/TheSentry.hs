module Arkham.Story.Cards.TheSentry (theSentry) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted hiding (story)
import Arkham.Message.Lifted.CreateEnemy
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheSentry = TheSentry StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSentry :: StoryCard TheSentry
theSentry = story TheSentry Cards.theSentry

instance RunMessage TheSentry where
  runMessage msg s@(TheSentry attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      sid <- getRandom
      beginSkillTest sid iid attrs iid #agility (Fixed 3)
      pure s
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      runCreateEnemyT Enemies.gugSentinel iid \sentinel -> do
        afterCreate $ placeClues attrs sentinel =<< perPlayer 1
      pure s
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      cityOfGugs <- selectJust $ locationIs Locations.cityOfGugs
      runCreateEnemyT Enemies.gugSentinel cityOfGugs \sentinel -> do
        createExhausted
        afterCreate $ placeClues attrs sentinel =<< perPlayer 1
      pure s
    _ -> TheSentry <$> liftRunMessage msg attrs
