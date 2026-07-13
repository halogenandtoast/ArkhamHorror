module Arkham.Agenda.Cards.SavageNatureCircusExMortis (savageNatureCircusExMortis) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype SavageNatureCircusExMortis = SavageNatureCircusExMortis AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

savageNatureCircusExMortis :: AgendaCard SavageNatureCircusExMortis
savageNatureCircusExMortis =
  agenda (1, A) SavageNatureCircusExMortis Cards.savageNatureCircusExMortis (Static 7)

-- Front: "Adjacent copies of Moonlit Forest are connected to each other." This
-- is realized by the Moonlit Forest locations' connectsToAdjacent placement.

instance RunMessage SavageNatureCircusExMortis where
  runMessage msg a@(SavageNatureCircusExMortis attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      illusionist <- getSetAsideCard Enemies.newMoonIllusionistCircusExMortis
      camp <- selectJust $ locationIs Locations.circusEncampmentCircusExMortis
      createEnemyAt_ illusionist camp
      advanceAgendaDeck attrs
      pure a
    _ -> SavageNatureCircusExMortis <$> liftRunMessage msg attrs
