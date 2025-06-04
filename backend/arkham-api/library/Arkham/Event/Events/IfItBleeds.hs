module Arkham.Event.Events.IfItBleeds (ifItBleeds) where

import Arkham.Enemy.Types (Field (EnemySanityDamage))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (EnemyDefeated)
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Window

newtype IfItBleeds = IfItBleeds EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ifItBleeds :: EventCard IfItBleeds
ifItBleeds = event IfItBleeds Cards.ifItBleeds

getWindowEnemyIds :: InvestigatorId -> [Window] -> [EnemyId]
getWindowEnemyIds iid = mapMaybe \case
  Window Timing.After (EnemyDefeated (Just who) _ eid) _ | iid == who -> Just eid
  _ -> Nothing

instance RunMessage IfItBleeds where
  runMessage msg e@(IfItBleeds attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let enemyIds = getWindowEnemyIds iid attrs.windows
      chooseOneM iid do
        targets enemyIds $ \enemyId -> do
          horrorValue <- field EnemySanityDamage enemyId
          investigators <- select $ HealableInvestigator (toSource attrs) #horror $ colocatedWith iid
          for_ investigators \investigator -> healHorror investigator attrs horrorValue
      pure e
    _ -> IfItBleeds <$> liftRunMessage msg attrs
