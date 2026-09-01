module Arkham.Homebrew.CircusExMortis.Agendas.TheTrueFace (theTrueFace) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Matcher

newtype TheTrueFace = TheTrueFace AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueFace :: AgendaCard TheTrueFace
theTrueFace = agenda (1, A) TheTrueFace Cards.theTrueFace (Static 6)

instance RunMessage TheTrueFace where
  runMessage msg a@(TheTrueFace attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      createSetAsideEnemy_ Enemies.disguisedMonstrosity $ location_ "The Big Top"
      advanceAgendaDeck attrs
      pure a
    _ -> TheTrueFace <$> liftRunMessage msg attrs
