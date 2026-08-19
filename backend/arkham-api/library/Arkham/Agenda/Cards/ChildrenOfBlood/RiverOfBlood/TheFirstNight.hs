module Arkham.Agenda.Cards.ChildrenOfBlood.RiverOfBlood.TheFirstNight (theFirstNight) where

import Arkham.Agenda.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheFirstNight = TheFirstNight AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFirstNight :: AgendaCard TheFirstNight
theFirstNight = agenda (2, A) TheFirstNight Cards.theFirstNight (Static 5)

instance RunMessage TheFirstNight where
  runMessage msg a@(TheFirstNight attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheFirstNight <$> liftRunMessage msg attrs
