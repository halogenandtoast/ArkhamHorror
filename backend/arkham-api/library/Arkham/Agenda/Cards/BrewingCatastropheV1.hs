module Arkham.Agenda.Cards.BrewingCatastropheV1 (brewingCatastropheV1) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype BrewingCatastropheV1 = BrewingCatastropheV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brewingCatastropheV1 :: AgendaCard BrewingCatastropheV1
brewingCatastropheV1 = agenda (1, A) BrewingCatastropheV1 Cards.brewingCatastropheV1 (Static 12)

instance RunMessage BrewingCatastropheV1 where
  runMessage msg a@(BrewingCatastropheV1 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> BrewingCatastropheV1 <$> liftRunMessage msg attrs
