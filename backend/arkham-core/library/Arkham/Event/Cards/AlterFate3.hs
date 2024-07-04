module Arkham.Event.Cards.AlterFate3 (alterFate3, AlterFate3 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Message qualified as Msg
import Arkham.Matcher

newtype AlterFate3 = AlterFate3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alterFate3 :: EventCard AlterFate3
alterFate3 = event AlterFate3 Cards.alterFate3

instance RunMessage AlterFate3 where
  runMessage msg e@(AlterFate3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      treacheries <- select $ NotTreachery (TreacheryOnEnemy EliteEnemy) <> TreacheryIsNonWeakness
      chooseOne iid $ targetLabels treacheries $ only . Msg.toDiscardBy iid attrs
      pure e
    _ -> AlterFate3 <$> liftRunMessage msg attrs
