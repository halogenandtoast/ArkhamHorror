-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Agendas.TooSwiftArrives (tooSwiftArrives) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.NightOfVespers.CardDefs.Agendas qualified as Cards

newtype TooSwiftArrives = TooSwiftArrives AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tooSwiftArrives :: AgendaCard TooSwiftArrives
tooSwiftArrives = agenda (1, A) TooSwiftArrives Cards.tooSwiftArrives (Static 1)

instance RunMessage TooSwiftArrives where
  runMessage msg (TooSwiftArrives attrs) = TooSwiftArrives <$> runMessage msg attrs
