module Arkham.Event.Events.ThinkOnYourFeet2 (thinkOnYourFeet2) where

import Arkham.Enemy.Types (Field (EnemySpawnDetails))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (getAccessibleLocations, getLocationOf)
import Arkham.Helpers.Window (getEnemy)
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Spawn

newtype ThinkOnYourFeet2 = ThinkOnYourFeet2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thinkOnYourFeet2 :: EventCard ThinkOnYourFeet2
thinkOnYourFeet2 = event ThinkOnYourFeet2 Cards.thinkOnYourFeet2

instance RunMessage ThinkOnYourFeet2 where
  runMessage msg e@(ThinkOnYourFeet2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      connectedLocations <- getAccessibleLocations iid attrs
      chooseTargetM iid connectedLocations (moveTo attrs iid)

      -- The enemy still spawns at your previous location.
      runMaybeT_ do
        let eid = getEnemy attrs.windows
        lid <- MaybeT $ getLocationOf iid
        details <- MaybeT $ field EnemySpawnDetails eid
        SpawnEngagedWith matcher <- hoistMaybe $ pure details.spawnAt
        -- If the enemy was only going to engage you we update it to the location instead
        liftGuardM $ (== 1) <$> selectCount matcher
        lift $ push $ UpdateEnemy eid (Update EnemySpawnDetails (Just $ details & setLocation lid))
      pure e
    _ -> ThinkOnYourFeet2 <$> liftRunMessage msg attrs
