module Arkham.Agenda.Cards.WhenItRains (whenItRains) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype WhenItRains = WhenItRains AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whenItRains :: AgendaCard WhenItRains
whenItRains =
  agendaWith (1, A) WhenItRains Cards.whenItRains (Static 2)
    $ removeDoomMatchersL
    %~ (\rdm -> rdm {removeDoomAgendas = NotAgenda AnyAgenda})

instance RunMessage WhenItRains where
  runMessage msg a@(WhenItRains attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        assignHorrorTo attrs 1 iid
        loseAllClues iid attrs
      placeSetAsideLocations_ [Locations.kensingtonGardens, Locations.westminsterAbbey, Locations.bigBen]
      lead <- getLead
      theRedGlovedMan <- fetchCard Enemies.theRedGlovedManShroudedInMystery
      drawCard lead theRedGlovedMan
      advanceToActA attrs Acts.theGameIsAfoot
      advanceAgendaDeck attrs
      placeDoomOnAgenda attrs.doom
      pure a
    _ -> WhenItRains <$> liftRunMessage msg attrs
