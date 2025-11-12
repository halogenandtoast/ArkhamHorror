module Arkham.Treachery.Cards.SuspiciousGazeA (suspiciousGazeA) where

import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SuspiciousGazeA = SuspiciousGazeA TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suspiciousGazeA :: TreacheryCard SuspiciousGazeA
suspiciousGazeA = treachery SuspiciousGazeA Cards.suspiciousGazeA

instance RunMessage SuspiciousGazeA where
  runMessage msg t@(SuspiciousGazeA attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      x <- getAlarmLevel iid
      let n = x + 1 `div` 2
      chooseOneM iid $ scenarioI18n do
        unscoped $ countVar n $ labeledValidate' (n > 0) "takeDamage" $ assignDamage iid attrs n
        labeledValidate' (n < 10) "suspiciousGaze.alarm" $ raiseAlarmLevel attrs [iid]
      pure t
    _ -> SuspiciousGazeA <$> liftRunMessage msg attrs
