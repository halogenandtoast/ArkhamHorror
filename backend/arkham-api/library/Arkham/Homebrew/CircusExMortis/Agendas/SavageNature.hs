module Arkham.Homebrew.CircusExMortis.Agendas.SavageNature (savageNature) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Matcher

newtype SavageNature = SavageNature AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

savageNature :: AgendaCard SavageNature
savageNature =
  agenda (1, A) SavageNature Cards.savageNature (Static 7)

-- Front: "Adjacent copies of Moonlit Forest are connected to each other." This
-- is realized by the Moonlit Forest locations' connectsToAdjacent placement.

instance RunMessage SavageNature where
  runMessage msg a@(SavageNature attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      illusionist <- getSetAsideCard Enemies.newMoonIllusionist
      camp <- selectJust $ locationIs Locations.circusEncampment
      createEnemyAt_ illusionist camp
      advanceAgendaDeck attrs
      pure a
    _ -> SavageNature <$> liftRunMessage msg attrs
