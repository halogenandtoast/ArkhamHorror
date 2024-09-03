module Arkham.Event.Cards.StallForTime (stallForTime, StallForTime (..)) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher
import Arkham.Modifier hiding (EnemyEvade, EnemyFight)
import Arkham.Phase
import Arkham.Projection

newtype StallForTime = StallForTime EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stallForTime :: EventCard StallForTime
stallForTime = event StallForTime Cards.stallForTime

instance RunMessage StallForTime where
  runMessage msg e@(StallForTime attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ enemyAtLocationWith iid <> oneOf [EnemyWithFight, EnemyWithEvade]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      mEvade <- field EnemyEvade eid
      mFight <- field EnemyFight eid
      let fld =
            fromJustNote "must have one" (align mEvade mFight) & \case
              This _ -> EnemyEvade
              That _ -> EnemyFight
              These evd fgt -> if evd < fgt then EnemyEvade else EnemyFight
      sid <- getRandom
      parley sid iid attrs eid #willpower $ EnemyMaybeFieldCalculation eid fld
      pure e
    PassedThisSkillTest _iid (isSource attrs -> True) -> do
      whenJustM getSkillTestTarget \case
        EnemyTarget eid -> do
          push $ Exhaust (toTarget eid)
          whenM (eid <=~> NonEliteEnemy) do
            nextPhaseModifier UpkeepPhase attrs eid DoesNotReadyDuringUpkeep
        _ -> pure ()
      pure e
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      returnToHand iid attrs
      pure e
    _ -> StallForTime <$> liftRunMessage msg attrs
