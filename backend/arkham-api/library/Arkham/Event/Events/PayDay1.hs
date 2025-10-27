module Arkham.Event.Events.PayDay1 (payDay1) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.History
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Taboo

newtype PayDay1 = PayDay1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

payDay1 :: EventCard PayDay1
payDay1 = event PayDay1 Cards.payDay1

instance RunMessage PayDay1 where
  runMessage msg e@(PayDay1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      n <-
        if tabooed TabooList22 attrs
          then getHistoryField TurnHistory iid HistoryActionsSpent
          else fieldMap InvestigatorActionsPerformed ((+ 1) . length) iid
      takeResources iid attrs n
      pure e
    _ -> PayDay1 <$> liftRunMessage msg attrs
