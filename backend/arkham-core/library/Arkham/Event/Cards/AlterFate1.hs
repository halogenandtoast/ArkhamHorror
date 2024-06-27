module Arkham.Event.Cards.AlterFate1 (alterFate1, AlterFate1 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Message qualified as Msg
import Arkham.Matcher

newtype AlterFate1 = AlterFate1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alterFate1 :: EventCard AlterFate1
alterFate1 = event AlterFate1 Cards.alterFate1

instance RunMessage AlterFate1 where
  runMessage msg e@(AlterFate1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      treacheries <- select $ NotTreachery (TreacheryOnEnemy EliteEnemy) <> TreacheryIsNonWeakness
      chooseOne iid $ targetLabels treacheries $ only . Msg.toDiscardBy iid attrs
      pure e
    _ -> AlterFate1 <$> lift (runMessage msg attrs)
