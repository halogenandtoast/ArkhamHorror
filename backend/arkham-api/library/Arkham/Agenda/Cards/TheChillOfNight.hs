module Arkham.Agenda.Cards.TheChillOfNight (TheChillOfNight (..), theChillOfNight) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype TheChillOfNight = TheChillOfNight AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChillOfNight :: AgendaCard TheChillOfNight
theChillOfNight = agenda (5, A) TheChillOfNight Cards.theChillOfNight (Static 5)

instance RunMessage TheChillOfNight where
  runMessage msg a@(TheChillOfNight attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      chooseSelectM lead (LocationWithCardsUnderneath AnyCards) (handleTarget lead attrs)
      advanceAgendaDeck attrs
      pure a
    HandleTargetChoice _ (isSource attrs -> True) (LocationTarget location) -> do
      lead <- getLead
      cards <- field LocationCardsUnderneath location
      focusCards cards (continue lead . pure)
      pure a
    _ -> TheChillOfNight <$> liftRunMessage msg attrs
