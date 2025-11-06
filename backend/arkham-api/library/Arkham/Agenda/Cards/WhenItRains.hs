module Arkham.Agenda.Cards.WhenItRains (whenItRains) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype WhenItRains = WhenItRains AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whenItRains :: AgendaCard WhenItRains
whenItRains =
  agendaWith (1, A) WhenItRains Cards.whenItRains (Static 2)
    $ removeDoomMatchersL
    %~ (\rdm -> rdm {removeDoomAgendas = NotAgenda AnyAgenda})

-- Ability is no-op just to draw attention to it
instance HasAbilities WhenItRains where
  getAbilities (WhenItRains a) = [mkAbility a 1 $ forced $ AgendaAdvances #when (AgendaWithDoom $ atLeast 1)]

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
