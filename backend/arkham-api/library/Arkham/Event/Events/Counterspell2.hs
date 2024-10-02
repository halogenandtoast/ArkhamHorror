module Arkham.Event.Events.Counterspell2 (counterspell2, Counterspell2 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (getChaosToken)

newtype Counterspell2 = Counterspell2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

counterspell2 :: EventCard Counterspell2
counterspell2 = event Counterspell2 Cards.counterspell2

instance RunMessage Counterspell2 where
  runMessage msg e@(Counterspell2 attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent _ (is attrs -> True) _ (getChaosToken -> token) _ -> do
      cancelChaosToken attrs token
      pure e
    _ -> Counterspell2 <$> liftRunMessage msg attrs
