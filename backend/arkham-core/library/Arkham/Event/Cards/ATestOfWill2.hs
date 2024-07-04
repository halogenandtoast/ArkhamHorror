module Arkham.Event.Cards.ATestOfWill2 (aTestOfWill2, ATestOfWill2 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype ATestOfWill2 = ATestOfWill2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTestOfWill2 :: EventCard ATestOfWill2
aTestOfWill2 = event ATestOfWill2 Cards.aTestOfWill2

instance RunMessage ATestOfWill2 where
  runMessage msg e@(ATestOfWill2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      push $ CancelRevelation (toSource attrs)
      beginSkillTest iid attrs iid #willpower (Fixed 3)
      pure e
    FailedThisSkillTest _ (isSource attrs -> True) -> do
      push $ Exile (toTarget attrs)
      pure e
    _ -> ATestOfWill2 <$> liftRunMessage msg attrs
