module Arkham.Event.Events.JoinTheCaravan1 (joinTheCaravan1, JoinTheCaravan1 (..)) where

import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Calculation
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Movement

newtype JoinTheCaravan1 = JoinTheCaravan1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

joinTheCaravan1 :: EventCard JoinTheCaravan1
joinTheCaravan1 = event JoinTheCaravan1 Cards.joinTheCaravan1

instance HasModifiersFor JoinTheCaravan1 where
  getModifiersFor (JoinTheCaravan1 a) = do
    n <- calculate (DifferentClassAmong $ ControlledBy $ HandWith $ HasCard $ CardWithId $ toCardId a)
    modified_ a (CardIdTarget $ toCardId a) [ReduceCostOf (CardWithId $ toCardId a) n]

instance RunMessage JoinTheCaravan1 where
  runMessage msg e@(JoinTheCaravan1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      locations <- select $ CanMoveToLocation (InvestigatorWithId iid) (toSource attrs) RevealedLocation
      chooseOrRunOne
        iid
        [targetLabel location [Move $ move (toSource attrs) iid location] | location <- locations]
      pure e
    _ -> JoinTheCaravan1 <$> liftRunMessage msg attrs
