module Arkham.Treachery.Cards.SuspiciousGazeC (suspiciousGazeC) where

import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SuspiciousGazeC = SuspiciousGazeC TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suspiciousGazeC :: TreacheryCard SuspiciousGazeC
suspiciousGazeC = treachery SuspiciousGazeC Cards.suspiciousGazeC

instance RunMessage SuspiciousGazeC where
  runMessage msg t@(SuspiciousGazeC attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      x <- getAlarmLevel iid
      let n = (x + 1) `div` 2
      chooseOneM iid $ scenarioI18n do
        unscoped $ countVar n $ labeledValidate' (n > 0) "takeDamage" $ assignDamage iid attrs n
        labeledValidate' (n < 10) "suspiciousGaze.alarm" $ raiseAlarmLevel attrs [iid]
      pure t
    _ -> SuspiciousGazeC <$> liftRunMessage msg attrs
