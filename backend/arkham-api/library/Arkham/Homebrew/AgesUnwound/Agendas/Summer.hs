module Arkham.Homebrew.AgesUnwound.Agendas.Summer (summer) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards

newtype Summer = Summer AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /Summer/ (@:ages-unwound:109@), the last agenda of the year. No printed
front text: the year simply runs out.
-}
summer :: AgendaCard Summer
summer = agenda (4, A) Summer Cards.summer (Static 5)

instance RunMessage Summer where
  runMessage msg a@(Summer attrs) = runQueueT $ case msg of
    -- "__The Day of Reckoning__ - ->R1."
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R1
      pure a
    _ -> Summer <$> liftRunMessage msg attrs
