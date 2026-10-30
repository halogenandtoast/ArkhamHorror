-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Agendas.OpenSeason (openSeason) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.NightOfVespers.CardDefs.Agendas qualified as Cards

newtype OpenSeason = OpenSeason AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openSeason :: AgendaCard OpenSeason
openSeason = agenda (2, A) OpenSeason Cards.openSeason (Static 6)

instance RunMessage OpenSeason where
  runMessage msg (OpenSeason attrs) = OpenSeason <$> runMessage msg attrs
