module Arkham.Agenda.Cards.UnderTheSurface
  ( UnderTheSurface(..)
  , underTheSurface
  ) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype UnderTheSurface = UnderTheSurface AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underTheSurface :: AgendaCard UnderTheSurface
underTheSurface = agenda (1, A) UnderTheSurface Cards.underTheSurface (Static 6)

instance RunMessage UnderTheSurface where
  runMessage msg a@(UnderTheSurface attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> UnderTheSurface <$> liftRunMessage msg attrs
