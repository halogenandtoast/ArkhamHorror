module Arkham.Agenda.Cards.StirringInTheDark (stirringInTheDark) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype StirringInTheDark = StirringInTheDark AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stirringInTheDark :: AgendaCard StirringInTheDark
stirringInTheDark = agenda (1, A) StirringInTheDark Cards.stirringInTheDark (Static 12)

-- TODO: abilities

instance RunMessage StirringInTheDark where
  runMessage msg (StirringInTheDark attrs) = runQueueT $ StirringInTheDark <$> liftRunMessage msg attrs
