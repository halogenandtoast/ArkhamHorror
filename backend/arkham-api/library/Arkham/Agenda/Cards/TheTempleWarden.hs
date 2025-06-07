module Arkham.Agenda.Cards.TheTempleWarden (theTempleWarden) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TheTempleWarden = TheTempleWarden AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTempleWarden :: AgendaCard TheTempleWarden
theTempleWarden = agenda (2, A) TheTempleWarden Cards.theTempleWarden (StaticWithPerPlayer 12 1)

instance RunMessage TheTempleWarden where
  runMessage msg a@(TheTempleWarden attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator (investigatorDefeated attrs)
      pure a
    _ -> TheTempleWarden <$> liftRunMessage msg attrs
