module Arkham.Event.Events.TheStarsAreRight (theStarsAreRight) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Query

newtype TheStarsAreRight = TheStarsAreRight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStarsAreRight :: EventCard TheStarsAreRight
theStarsAreRight = event TheStarsAreRight Cards.theStarsAreRight

instance RunMessage TheStarsAreRight where
  runMessage msg e@(TheStarsAreRight attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- getInvestigators
      push $ RemoveEvent attrs.id
      chooseOrRunOneM iid do
        targets investigators \investigator -> do
          drawCards investigator (toSource attrs) 1
          gainResources investigator (toSource attrs) 1
          takeActionAsIfTurn investigator attrs
      pure e
    InvestigatorPlayEvent _ eid _ _ _ | eid == toId attrs -> error "Unplayable"
    _ -> TheStarsAreRight <$> liftRunMessage msg attrs
