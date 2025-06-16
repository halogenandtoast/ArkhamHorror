module Arkham.Event.Events.TheStarsAreRight (theStarsAreRight) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Query
import Arkham.Matcher

newtype TheStarsAreRight = TheStarsAreRight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStarsAreRight :: EventCard TheStarsAreRight
theStarsAreRight = event TheStarsAreRight Cards.theStarsAreRight

instance RunMessage TheStarsAreRight where
  runMessage msg e@(TheStarsAreRight attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- select $ affectsOthers UneliminatedInvestigator
      iid' <- getActiveInvestigatorId
      push $ RemoveEvent attrs.id
      chooseOrRunOneM iid do
        targets investigators \investigator -> do
          drawCards investigator (toSource attrs) 1
          gainResources investigator (toSource attrs) 1
          when (iid /= iid') $ push $ SetActiveInvestigator iid
          takeActionAsIfTurn investigator attrs
          when (iid /= iid') $ push $ SetActiveInvestigator iid'
      pure e
    InvestigatorPlayEvent _ eid _ _ _ | eid == toId attrs -> error "Unplayable"
    _ -> TheStarsAreRight <$> liftRunMessage msg attrs
