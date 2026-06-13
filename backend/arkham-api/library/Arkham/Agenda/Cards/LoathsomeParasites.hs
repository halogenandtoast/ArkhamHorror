module Arkham.Agenda.Cards.LoathsomeParasites (loathsomeParasites) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype LoathsomeParasites = LoathsomeParasites AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loathsomeParasites :: AgendaCard LoathsomeParasites
loathsomeParasites = agenda (2, A) LoathsomeParasites Cards.loathsomeParasites (Static 6)

-- TODO: abilities

instance RunMessage LoathsomeParasites where
  runMessage msg (LoathsomeParasites attrs) = runQueueT $ LoathsomeParasites <$> liftRunMessage msg attrs
