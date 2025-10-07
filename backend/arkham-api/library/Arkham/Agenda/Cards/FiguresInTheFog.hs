module Arkham.Agenda.Cards.FiguresInTheFog (figuresInTheFog) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Zone

newtype FiguresInTheFog = FiguresInTheFog AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

figuresInTheFog :: AgendaCard FiguresInTheFog
figuresInTheFog =
  agendaWith (2, A) FiguresInTheFog Cards.figuresInTheFog (Static 4)
    $ removeDoomMatchersL
    %~ (\rdm -> rdm {removeDoomAgendas = NotAgenda AnyAgenda})

instance RunMessage FiguresInTheFog where
  runMessage msg a@(FiguresInTheFog attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      markTime 1
      selectEach (enemyIs Enemies.theRedGlovedManShroudedInMystery) (`place` SetAsideZone)
      removeAllConcealed
      placeSetAsideLocations_ [Locations.theTowerBridge, Locations.towerOfLondon]
      shuffleEncounterDiscardBackIn
      shuffleSetAsideEncounterSetIntoEncounterDeck Set.CrimsonConspiracy
      shuffleSetAsideEncounterSetIntoEncounterDeck Set.Outsiders
      eachInvestigator (`loseAllClues` attrs)
      advanceToActA attrs Acts.eyesInTheTower
      advanceAgendaDeck attrs
      placeDoomOnAgenda attrs.doom
      pure a
    _ -> FiguresInTheFog <$> liftRunMessage msg attrs
