module Arkham.Treachery.Cards.AvariceCallsA (avariceCallsA) where

import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AvariceCallsA = AvariceCallsA TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

avariceCallsA :: TreacheryCard AvariceCallsA
avariceCallsA = treachery AvariceCallsA Cards.avariceCallsA

instance RunMessage AvariceCallsA where
  runMessage msg t@(AvariceCallsA attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- getAlarmLevel iid
      chooseOneM iid $ scenarioI18n do
        unscoped $ countVar 2 $ labeled' "takeHorror" $ assignHorror iid attrs 2
        labeled' "avariceCalls.test" do
          sid <- getRandom
          revelationSkillTest sid iid attrs #willpower (Fixed $ (n + 1) `div` 2)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      gainResources iid attrs 2
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      raiseAlarmLevel attrs [iid]
      pure t
    _ -> AvariceCallsA <$> liftRunMessage msg attrs
