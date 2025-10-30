module Arkham.Event.Events.ThinkOnYourFeet (thinkOnYourFeet) where

import Arkham.Enemy.Types (Field (EnemySpawnDetails))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (getAccessibleLocations, getLocationOf)
import Arkham.Helpers.Window (getEnemy)
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Spawn

newtype ThinkOnYourFeet = ThinkOnYourFeet EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thinkOnYourFeet :: EventCard ThinkOnYourFeet
thinkOnYourFeet = event ThinkOnYourFeet Cards.thinkOnYourFeet

instance RunMessage ThinkOnYourFeet where
  runMessage msg e@(ThinkOnYourFeet attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      -- Immediately move to a connecting location.
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
    _ -> ThinkOnYourFeet <$> liftRunMessage msg attrs
