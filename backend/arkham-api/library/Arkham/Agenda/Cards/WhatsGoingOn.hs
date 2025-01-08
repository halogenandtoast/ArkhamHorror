module Arkham.Agenda.Cards.WhatsGoingOn (WhatsGoingOn(..), whatsGoingOn) where

-- Constructor is only exported for testing purposes

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Matcher
import Arkham.Helpers.Query (getLead)
import Arkham.Message.Lifted.Choose

newtype WhatsGoingOn = WhatsGoingOn AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatsGoingOn :: AgendaCard WhatsGoingOn
whatsGoingOn = agenda (1, A) WhatsGoingOn Cards.whatsGoingOn (Static 3)

instance RunMessage WhatsGoingOn where
  runMessage msg a@(WhatsGoingOn attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      chooseOneM lead do
        labeled "The lead investigator takes 2 horror" $ assignHorror lead attrs 2

        whenAny InvestigatorWithNonEmptyHand do
          labeled "Each investigator discards 1 card at random from his or her hand" do
            push $ AllRandomDiscard (toSource attrs) AnyCard
      advanceAgendaDeck attrs
      pure a
    _ -> WhatsGoingOn <$> liftRunMessage msg attrs
