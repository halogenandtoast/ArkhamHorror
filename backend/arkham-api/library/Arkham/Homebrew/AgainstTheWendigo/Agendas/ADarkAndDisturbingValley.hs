module Arkham.Homebrew.AgainstTheWendigo.Agendas.ADarkAndDisturbingValley (
  aDarkAndDisturbingValley,
) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Agendas qualified as Cards

newtype ADarkAndDisturbingValley = ADarkAndDisturbingValley AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aDarkAndDisturbingValley :: AgendaCard ADarkAndDisturbingValley
aDarkAndDisturbingValley =
  agenda (1, A) ADarkAndDisturbingValley Cards.aDarkAndDisturbingValley (Static 4)

instance RunMessage ADarkAndDisturbingValley where
  runMessage msg a@(ADarkAndDisturbingValley attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> ADarkAndDisturbingValley <$> liftRunMessage msg attrs
