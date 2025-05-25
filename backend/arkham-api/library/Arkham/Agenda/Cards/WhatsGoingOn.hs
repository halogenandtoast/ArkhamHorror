module Arkham.Agenda.Cards.WhatsGoingOn (WhatsGoingOn (..), whatsGoingOn) where

-- Constructor is only exported for testing purposes

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheGathering.Helpers

newtype WhatsGoingOn = WhatsGoingOn AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatsGoingOn :: AgendaCard WhatsGoingOn
whatsGoingOn = agenda (1, A) WhatsGoingOn Cards.whatsGoingOn (Static 3)

instance RunMessage WhatsGoingOn where
  runMessage msg a@(WhatsGoingOn attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> scenarioI18n do
      lead <- getLead
      chooseOneM lead do
        labeled' "whatsGoingOn.horror" $ assignHorror lead attrs 2

        whenAny InvestigatorWithNonEmptyHand do
          labeled' "whatsGoingOn.discard" $ push $ AllRandomDiscard (toSource attrs) AnyCard
      advanceAgendaDeck attrs
      pure a
    _ -> WhatsGoingOn <$> liftRunMessage msg attrs
