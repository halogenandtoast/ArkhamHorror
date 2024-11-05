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
import Arkham.Location.Types (Field(..))
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
  getAbilities (UnchangingAsTheSea a) =
    guard (onSide A a)
      *> [ mkAbility a 1 $ forced $ ScenarioEvent #when "captured"
         , mkAbility a 2 $ forced $ AgendaAdvances #when (AgendaWithId a.id)
         ]

instance RunMessage UnchangingAsTheSea where
  runMessage msg a@(UnchangingAsTheSea attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ AdvanceAgenda attrs.id
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        captured iid
        push $ ForInvestigator iid msg -- holding cells might not be in play so we delay
      selectForMaybeM (InPlayEnemy $ enemyIs Enemies.oceirosMarsh) \oceiros -> do
        removeFromGame oceiros
        setCardAside =<< field EnemyCard oceiros
      shuffleSetAsideIntoEncounterDeck [Treacheries.takenCaptive]

      basement <- selectJust $ locationIs Locations.sunkenGrottoUpperDepths
      reveal basement

      void $ placeLocationCardInGrid (Pos 2 (-2)) Locations.sunkenGrottoLowerDepths
      void $ placeLocationCardInGrid (Pos 2 (-3)) Locations.sunkenGrottoFinalDepths

      {- FOURMOLU_DISABLE -}
      let
        positions =
          [ Pos 0 (-1), Pos 1 (-1), Pos 3 (-1)
          , Pos 0 (-2), Pos 1 (-2), Pos 3 (-2)
          , Pos 0 (-3), Pos 1 (-3), Pos 3 (-3)
          ]
      {- FOURMOLU_ENABLE -}

      tidalTunnels <- shuffle =<< getSetAsideCardsMatching (CardWithTitle "Tidal Tunnel")

      for_ (zip positions tidalTunnels) \(pos, tunnel) -> do
        loc <- placeLocationInGrid pos tunnel
        push $ UpdateLocation loc (Update LocationConnectsTo mempty)
        push $ UpdateLocation loc (Update LocationConnectedMatchers [LocationInRow pos.row])
        push $ UpdateLocation loc (Update LocationRevealedConnectedMatchers [LocationInRow pos.row])

      push $ AdvanceToAct 1 Acts.worshippersOfTheDeep Act.A (toSource attrs)
      advanceAgendaDeck attrs
      placeDoomOnAgenda attrs.doom
      pure a
    ForInvestigator iid (AdvanceAgenda (isSide B attrs -> True)) -> do
      holdingCells <- selectJust $ locationIs Locations.holdingCells
      field InvestigatorKeys iid >>= traverse_ (placeKey holdingCells)
      pure a
    _ -> UnchangingAsTheSea <$> liftRunMessage msg attrs
