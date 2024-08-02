module Arkham.Event.Cards.Confound3 (confound3, Confound3 (..)) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Modifier hiding (EnemyEvade)

newtype Confound3 = Confound3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

confound3 :: EventCard Confound3
confound3 = event Confound3 Cards.confound3

instance RunMessage Confound3 where
  runMessage msg e@(Confound3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ enemyAtLocationWith iid <> EnemyWithEvade
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      sid <- getRandom
      parley sid iid attrs eid #intellect $ EnemyMaybeFieldCalculation eid EnemyEvade
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      discoverAtYourLocationAndThen NotInvestigate iid attrs 2 do
        getSkillTestTarget >>= \case
          Just (EnemyTarget eid) -> do
            whenM (eid <=~> NonEliteEnemy) do
              automaticallyEvadeEnemy iid eid
              nextPhaseModifier #upkeep attrs eid DoesNotReadyDuringUpkeep
          _ -> pure ()

      pure e
    _ -> Confound3 <$> liftRunMessage msg attrs
