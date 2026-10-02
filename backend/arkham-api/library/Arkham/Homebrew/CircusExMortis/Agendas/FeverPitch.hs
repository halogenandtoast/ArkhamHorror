module Arkham.Homebrew.CircusExMortis.Agendas.FeverPitch (feverPitch) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (sufferTraumaAndDefeat)

newtype FeverPitch = FeverPitch AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feverPitch :: AgendaCard FeverPitch
feverPitch = agenda (3, A) FeverPitch Cards.feverPitch (Static 6)

instance RunMessage FeverPitch where
  runMessage msg a@(FeverPitch attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      sufferTraumaAndDefeat attrs
      pure a
    _ -> FeverPitch <$> liftRunMessage msg attrs
