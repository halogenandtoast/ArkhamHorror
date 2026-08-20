module Arkham.Agenda.Cards.ChildrenOfBlood.RiverOfBlood.TheSecondNight (theSecondNight) where

import Arkham.Agenda.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheSecondNight = TheSecondNight AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSecondNight :: AgendaCard TheSecondNight
theSecondNight = agenda (4, A) TheSecondNight Cards.theSecondNight (Static 5)

instance RunMessage TheSecondNight where
  runMessage msg a@(TheSecondNight attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheSecondNight <$> liftRunMessage msg attrs
