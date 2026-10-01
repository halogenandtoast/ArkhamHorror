module Arkham.Homebrew.CircusExMortis.Agendas.FadingSunlightVI (fadingSunlightVI) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards

newtype FadingSunlightVI = FadingSunlightVI AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Front prints no ability. Back ("Darkness Falls") is only "(→R1)".
fadingSunlightVI :: AgendaCard FadingSunlightVI
fadingSunlightVI = agenda (1, A) FadingSunlightVI Cards.fadingSunlightVI (Static 17)

instance RunMessage FadingSunlightVI where
  runMessage msg a@(FadingSunlightVI attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R1
      pure a
    _ -> FadingSunlightVI <$> liftRunMessage msg attrs
