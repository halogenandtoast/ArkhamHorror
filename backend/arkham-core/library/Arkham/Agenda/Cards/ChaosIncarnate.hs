module Arkham.Agenda.Cards.ChaosIncarnate (ChaosIncarnate (..), chaosIncarnate) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype ChaosIncarnate = ChaosIncarnate AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaosIncarnate :: AgendaCard ChaosIncarnate
chaosIncarnate = agenda (3, A) ChaosIncarnate Cards.chaosIncarnate (Static 5)

instance RunMessage ChaosIncarnate where
  runMessage msg a@(ChaosIncarnate attrs) = runQueueT $ case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      eachInvestigator \iid -> do
        push $ InvestigatorDefeated (toSource attrs) iid
        push $ DrivenInsane iid
      pure a
    _ -> ChaosIncarnate <$> lift (runMessage msg attrs)
