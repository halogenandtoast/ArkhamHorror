module Arkham.Event.Events.MakeEmSing (makeEmSing) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher

newtype MakeEmSing = MakeEmSing EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

makeEmSing :: EventCard MakeEmSing
makeEmSing = event MakeEmSing Cards.makeEmSing

instance RunMessage MakeEmSing where
  runMessage msg e@(MakeEmSing attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid \location -> do
        enemies <- select $ enemyAt location <> canParleyEnemy iid
        sid <- getRandom
        chooseTargetM iid enemies \enemy -> do
          parley sid iid attrs enemy #combat (EnemyMaybeFieldCalculation enemy EnemyRemainingHealth)
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      withSkillTestTargetedEnemy $ automaticallyEvadeEnemy iid
      discoverAtYourLocation NotInvestigate iid attrs 1
      pure e
    _ -> MakeEmSing <$> liftRunMessage msg attrs
