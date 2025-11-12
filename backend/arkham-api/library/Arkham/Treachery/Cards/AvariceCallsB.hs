module Arkham.Treachery.Cards.AvariceCallsB (avariceCallsB) where

import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AvariceCallsB = AvariceCallsB TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

avariceCallsB :: TreacheryCard AvariceCallsB
avariceCallsB = treachery AvariceCallsB Cards.avariceCallsB

instance RunMessage AvariceCallsB where
  runMessage msg t@(AvariceCallsB attrs) = runQueueT $ case msg of
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
    _ -> AvariceCallsB <$> liftRunMessage msg attrs
