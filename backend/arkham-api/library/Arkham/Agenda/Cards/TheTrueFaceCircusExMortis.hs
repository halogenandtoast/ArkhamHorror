module Arkham.Agenda.Cards.TheTrueFaceCircusExMortis (theTrueFaceCircusExMortis) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TheTrueFaceCircusExMortis = TheTrueFaceCircusExMortis AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueFaceCircusExMortis :: AgendaCard TheTrueFaceCircusExMortis
theTrueFaceCircusExMortis =
  agenda (1, A) TheTrueFaceCircusExMortis Cards.theTrueFaceCircusExMortis (Static 6)

instance RunMessage TheTrueFaceCircusExMortis where
  runMessage msg a@(TheTrueFaceCircusExMortis attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      bigTops <- select $ LocationWithTitle "The Big Top"
      chooseTargetM lead bigTops \lid ->
        createSetAsideEnemyWith_ Enemies.disguisedMonstrosityCircusExMortis lid id
      advanceAgendaDeck attrs
      pure a
    _ -> TheTrueFaceCircusExMortis <$> liftRunMessage msg attrs
