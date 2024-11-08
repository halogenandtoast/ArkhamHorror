module Arkham.Act.Cards.FindingThePath (FindingThePath (..), findingThePath) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Agenda.Types (Field (..))
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Field
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Location.Types (Field(..))
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype FindingThePath = FindingThePath ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

findingThePath :: ActCard FindingThePath
findingThePath = act (2, A) FindingThePath Cards.findingThePath Nothing

instance HasAbilities FindingThePath where
  getAbilities (FindingThePath attrs) | onSide A attrs = do
    [mkAbility attrs 1 $ Objective $ forced $ Enters #after Anyone $ locationIs Locations.sunkenGrottoUpperDepths]
  getAbilities _ = []

instance RunMessage FindingThePath where
  runMessage msg a@(FindingThePath attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectForMaybeM (enemyIs Enemies.oceirosMarsh) addToVictory
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

      shuffleSetAsideIntoEncounterDeck [Treacheries.takenCaptive]

      push $ AdvanceToAgenda 1 Agendas.theTideRisesALightInTheFog Agenda.A (toSource attrs)
      placeDoomOnAgenda . getSum =<< selectAgg Sum AgendaDoom AnyAgenda
      advanceActDeck attrs
      pure a
    _ -> FindingThePath <$> liftRunMessage msg attrs
