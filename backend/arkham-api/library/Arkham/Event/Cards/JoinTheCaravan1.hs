module Arkham.Event.Cards.JoinTheCaravan1 (joinTheCaravan1, JoinTheCaravan1 (..)) where

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
  getModifiersFor (CardIdTarget cid) (JoinTheCaravan1 a) | toCardId a == cid = do
    n <- calculate (DifferentClassAmong $ ControlledBy $ HandWith $ HasCard $ CardWithId cid)
    pure $ toModifiers a [ReduceCostOf (CardWithId cid) n]
  getModifiersFor _ _ = pure []

instance RunMessage JoinTheCaravan1 where
  runMessage msg e@(JoinTheCaravan1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      locations <- select $ CanMoveToLocation (InvestigatorWithId iid) (toSource attrs) RevealedLocation
      chooseOrRunOne
        iid
        [targetLabel location [Move $ move (toSource attrs) iid location] | location <- locations]
      pure e
    _ -> JoinTheCaravan1 <$> liftRunMessage msg attrs
