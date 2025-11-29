module Arkham.Agenda.Cards.BrewingCatastropheV2 (brewingCatastropheV2) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype BrewingCatastropheV2 = BrewingCatastropheV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brewingCatastropheV2 :: AgendaCard BrewingCatastropheV2
brewingCatastropheV2 = agenda (1, A) BrewingCatastropheV2 Cards.brewingCatastropheV2 (Static 12)

instance RunMessage BrewingCatastropheV2 where
  runMessage msg a@(BrewingCatastropheV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> BrewingCatastropheV2 <$> liftRunMessage msg attrs
