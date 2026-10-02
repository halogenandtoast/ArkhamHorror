module Arkham.Homebrew.CircusExMortis.Agendas.MesmericMagic (mesmericMagic) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (sufferTraumaAndDefeat)

newtype MesmericMagic = MesmericMagic AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mesmericMagic :: AgendaCard MesmericMagic
mesmericMagic = agenda (3, A) MesmericMagic Cards.mesmericMagic (Static 5)

instance RunMessage MesmericMagic where
  runMessage msg a@(MesmericMagic attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      sufferTraumaAndDefeat attrs
      pure a
    _ -> MesmericMagic <$> liftRunMessage msg attrs
