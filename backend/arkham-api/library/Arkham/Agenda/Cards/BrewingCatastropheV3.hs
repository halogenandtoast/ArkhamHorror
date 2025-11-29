module Arkham.Agenda.Cards.BrewingCatastropheV3 (brewingCatastropheV3) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype BrewingCatastropheV3 = BrewingCatastropheV3 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brewingCatastropheV3 :: AgendaCard BrewingCatastropheV3
brewingCatastropheV3 = agenda (1, A) BrewingCatastropheV3 Cards.brewingCatastropheV3 (Static 12)

instance RunMessage BrewingCatastropheV3 where
  runMessage msg a@(BrewingCatastropheV3 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> BrewingCatastropheV3 <$> liftRunMessage msg attrs
