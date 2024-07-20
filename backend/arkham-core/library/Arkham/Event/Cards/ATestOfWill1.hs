module Arkham.Event.Cards.ATestOfWill1 (aTestOfWill1, ATestOfWill1 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (cardDrawn)

newtype ATestOfWill1 = ATestOfWill1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTestOfWill1 :: EventCard ATestOfWill1
aTestOfWill1 = event ATestOfWill1 Cards.aTestOfWill1

instance RunMessage ATestOfWill1 where
  runMessage msg e@(ATestOfWill1 attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent _iid eid _ (cardDrawn -> card) _ | eid == toId attrs -> do
      cancelRevelation attrs card
      push $ Exile $ toTarget attrs
      pure e
    _ -> ATestOfWill1 <$> liftRunMessage msg attrs
