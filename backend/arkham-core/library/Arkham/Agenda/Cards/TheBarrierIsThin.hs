module Arkham.Agenda.Cards.TheBarrierIsThin (
  TheBarrierIsThin (..),
  theBarrierIsThin,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Trait

newtype TheBarrierIsThin = TheBarrierIsThin AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBarrierIsThin :: AgendaCard TheBarrierIsThin
theBarrierIsThin =
  agenda (2, A) TheBarrierIsThin Cards.theBarrierIsThin (Static 5)

instance RunMessage TheBarrierIsThin where
  runMessage msg a@(TheBarrierIsThin attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      tenochtitlanLocations <-
        selectList $ LocationWithTrait Tenochtitlan <> LocationWithoutClues
      iids <- getInvestigatorIds
      timeCollapsing <- getSetAsideCard Agendas.timeCollapsing
      theReturnTrip <- getSetAsideCard Acts.theReturnTrip

      pushAll
        $ map (AddToVictory . LocationTarget) tenochtitlanLocations
        <> [NextAdvanceAgendaStep (toId attrs) 1]
        <> map (InvestigatorDiscardAllClues (toSource attrs)) iids
        <> [NextAdvanceAgendaStep (toId attrs) 2]
        <> [ SetCurrentAgendaDeck 1 [timeCollapsing]
           , SetCurrentActDeck 1 [theReturnTrip]
           ]
      pure a
    NextAdvanceAgendaStep aid 1 | aid == toId attrs && onSide B attrs -> do
      presentDayLocations <- selectList $ LocationWithTrait PresentDay
      leadInvestigatorId <- getLeadInvestigatorId
      push
        $ chooseOneAtATime
          leadInvestigatorId
          [ targetLabel
            lid
            [ HandleTargetChoice
                leadInvestigatorId
                (toSource attrs)
                (LocationTarget lid)
            ]
          | lid <- presentDayLocations
          ]
      pure a
    HandleTargetChoice iid (isSource attrs -> True) (LocationTarget lid) -> do
      locationSymbol <- field LocationPrintedSymbol lid
      replacements <-
        filter ((== Just locationSymbol) . cdLocationRevealedSymbol . toCardDef)
          <$> getExplorationDeck
      pushAll
        [ FocusCards replacements
        , chooseOrRunOne
            iid
            [ TargetLabel
              (CardIdTarget $ toCardId replacement)
              [ RemoveCardFromScenarioDeck ExplorationDeck replacement
              , ReplaceLocation lid replacement DefaultReplace
              ]
            | replacement <- replacements
            ]
        , UnfocusCards
        ]
      pure a
    NextAdvanceAgendaStep aid 2 | aid == toId attrs && onSide B attrs -> do
      padma <- getSetAsideCard Enemies.padmaAmrita
      temploMayor <- selectJust $ LocationWithTitle "Templo Mayor"
      pushM $ createEnemyAt_ padma temploMayor Nothing
      pure a
    _ -> TheBarrierIsThin <$> runMessage msg attrs
