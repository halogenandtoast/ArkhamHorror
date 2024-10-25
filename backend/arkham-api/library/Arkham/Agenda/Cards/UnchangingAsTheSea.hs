module Arkham.Agenda.Cards.UnchangingAsTheSea (UnchangingAsTheSea (..), unchangingAsTheSea) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence qualified as Act
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyCard))
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Investigator.Types (Field (InvestigatorKeys))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.ALightInTheFog.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype UnchangingAsTheSea = UnchangingAsTheSea AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unchangingAsTheSea :: AgendaCard UnchangingAsTheSea
unchangingAsTheSea =
  agendaWith (2, A) UnchangingAsTheSea Cards.unchangingAsTheSea (Static 7)
    $ removeDoomMatchersL
    %~ (\rdm -> rdm {removeDoomAgendas = NotAgenda AnyAgenda})

-- Ability is no-op
instance HasAbilities UnchangingAsTheSea where
  getAbilities (UnchangingAsTheSea a) = [mkAbility a 2 $ forced $ AgendaAdvances #when (AgendaWithId a.id)]

-- Then, put random set-aside Tidal Tunnel locations into play until there are exactly 4 locations on each row, except for the row with the Lantern Room. (See Campaign Guide for diagram.)

instance RunMessage UnchangingAsTheSea where
  runMessage msg a@(UnchangingAsTheSea attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        captured iid
        push $ ForInvestigator iid msg -- holding cells might not be in play so we delay
      selectForMaybeM (enemyIs Enemies.oceirosMarsh) \oceiros -> do
        removeFromGame oceiros
        setCardAside =<< field EnemyCard oceiros
      shuffleSetAsideIntoEncounterDeck [Treacheries.takenCaptive]

      basement <- selectJust $ locationIs Locations.sunkenGrottoUpperDepths
      reveal basement

      void $ placeLocationCardInGrid (Pos 2 (-2)) Locations.sunkenGrottoLowerDepths
      void $ placeLocationCardInGrid (Pos 2 (-3)) Locations.sunkenGrottoFinalDepths

      let positions = [Pos 0 (-2), Pos 1 (-2), Pos 3 (-2), Pos 0 (-3), Pos 1 (-3), Pos 3 (-3)]

      tidalTunnels <- shuffle =<< getSetAsideCardsMatching (CardWithTitle "Tidal Tunnel")

      for_ (zip positions tidalTunnels) (void . uncurry placeLocationInGrid)

      push $ AdvanceToAct 1 Acts.worshippersOfTheDeep Act.A (toSource attrs)
      advanceAgendaDeck attrs
      placeDoomOnAgenda attrs.doom
      pure a
    ForInvestigator iid (AdvanceAgenda (isSide B attrs -> True)) -> do
      holdingCells <- selectJust $ locationIs Locations.holdingCells
      field InvestigatorKeys iid >>= traverse_ (placeKey holdingCells)
      pure a
    _ -> UnchangingAsTheSea <$> liftRunMessage msg attrs
