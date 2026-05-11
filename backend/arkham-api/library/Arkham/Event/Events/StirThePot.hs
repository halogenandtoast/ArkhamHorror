module Arkham.Event.Events.StirThePot (stirThePot) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Calculation
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Helpers.SkillTest.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype StirThePot = StirThePot EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stirThePot :: EventCard StirThePot
stirThePot = event StirThePot Cards.stirThePot

instance RunMessage StirThePot where
  runMessage msg e@(StirThePot attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ enemyAtLocationWith iid <> canParleyEnemy iid
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      parley_ iid attrs eid #intellect $ sumFieldsOf eid [EnemyHealthDamage, EnemySanityDamage]
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n -> do
      enemies <- select $ enemyAtLocationWith iid <> canBeDamagedBy attrs
      chooseOrRunOneAtATimeM iid $ targets enemies $ nonAttackEnemyDamage (Just iid) attrs 2
      when (n >= 2) $ doStep 1 msg
      pure e
    DoStep 1 (PassedThisSkillTest iid (isSource attrs -> True)) -> do
      engaged <- select $ enemyEngagedWith iid
      locations <- getAccessibleLocations iid attrs
      when (notNull locations) do
        chooseOrRunOneM iid $ withI18n do
          labeled' "doNotMove" nothing
          whenMatch iid InvestigatorCanDisengage $ labeled' "disengageAndMove" do
            for_ engaged (disengageEnemy iid)
            chooseTargetM iid locations (moveTo attrs iid)
            for_ engaged enemyCheckEngagement
          unlessMatch iid InvestigatorCanDisengage do
            targets locations (moveTo attrs iid)
      pure e
    _ -> StirThePot <$> liftRunMessage msg attrs
