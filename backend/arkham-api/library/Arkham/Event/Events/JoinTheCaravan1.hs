module Arkham.Event.Events.JoinTheCaravan1 (joinTheCaravan1) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Calculation
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype JoinTheCaravan1 = JoinTheCaravan1 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

joinTheCaravan1 :: EventCard JoinTheCaravan1
joinTheCaravan1 = event JoinTheCaravan1 Cards.joinTheCaravan1

instance HasModifiersFor JoinTheCaravan1 where
  getModifiersFor (JoinTheCaravan1 a) = do
    n <-
      calculate
        ( DifferentClassAmong (HandWith $ HasCard $ CardWithId a.cardId)
            $ ControlledBy
            $ HandWith
            $ HasCard
            $ CardWithId a.cardId
        )
    modified_ a a.cardId [ReduceCostOf (CardWithId a.cardId) n]

instance RunMessage JoinTheCaravan1 where
  runMessage msg e@(JoinTheCaravan1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      locations <- select $ CanMoveToLocation (InvestigatorWithId iid) (toSource attrs) RevealedLocation
      chooseOrRunOneM iid $ targets locations $ moveTo (toSource attrs) iid
      pure e
    _ -> JoinTheCaravan1 <$> liftRunMessage msg attrs
