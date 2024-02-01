module Arkham.Event.Cards.PayDay1 (
  payDay1,
  PayDay1 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype PayDay1 = PayDay1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

payDay1 :: EventCard PayDay1
payDay1 = event PayDay1 Cards.payDay1

instance RunMessage PayDay1 where
  runMessage msg e@(PayDay1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      n <- fieldMap InvestigatorActionsPerformed length iid
      pushAll [TakeResources iid n (toSource attrs) False]
      pure e
    _ -> PayDay1 <$> runMessage msg attrs
