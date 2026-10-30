-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Agendas.CallingItANight (callingItANight) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.NightOfVespers.CardDefs.Agendas qualified as Cards

newtype CallingItANight = CallingItANight AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callingItANight :: AgendaCard CallingItANight
callingItANight = agenda (3, A) CallingItANight Cards.callingItANight (Static 9)

instance RunMessage CallingItANight where
  runMessage msg (CallingItANight attrs) = CallingItANight <$> runMessage msg attrs
