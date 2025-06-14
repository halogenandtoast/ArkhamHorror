module Arkham.Agenda.Cards.TheBarrierIsThin (theBarrierIsThin) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (DefaultReplace))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Trait

newtype TheBarrierIsThin = TheBarrierIsThin AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBarrierIsThin :: AgendaCard TheBarrierIsThin
theBarrierIsThin = agenda (2, A) TheBarrierIsThin Cards.theBarrierIsThin (Static 5)

instance RunMessage TheBarrierIsThin where
  runMessage msg a@(TheBarrierIsThin attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      timeCollapsing <- getSetAsideCard Agendas.timeCollapsing
      theReturnTrip <- getSetAsideCard Acts.theReturnTrip
      selectEach (LocationWithTrait Tenochtitlan <> LocationWithoutClues) addToVictory
      doStep 1 msg
      eachInvestigator (discardAllClues attrs)
      doStep 2 msg
      push $ SetCurrentAgendaDeck 1 [timeCollapsing]
      push $ SetCurrentActDeck 1 [theReturnTrip]
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      presentDayLocations <- select $ LocationWithTrait PresentDay
      lead <- getLead
      chooseOneAtATimeM lead $ targets presentDayLocations $ handleTarget lead attrs
      pure a
    HandleTargetChoice iid (isSource attrs -> True) (LocationTarget lid) -> do
      locationSymbol <- field LocationPrintedSymbol lid
      replacements <-
        filter ((== Just locationSymbol) . cdLocationRevealedSymbol . toCardDef)
          <$> getExplorationDeck
      focusCards replacements do
        chooseOrRunOneM iid do
          targets replacements \replacement -> do
            push $ RemoveCardFromScenarioDeck ExplorationDeck replacement
            push $ ReplaceLocation lid replacement DefaultReplace
      pure a
    DoStep 2 (AdvanceAgenda (isSide B attrs -> True)) -> do
      temploMayor <- selectJust $ LocationWithTitle "Templo Mayor"
      createEnemyAt_ Enemies.padmaAmrita temploMayor
      pure a
    _ -> TheBarrierIsThin <$> liftRunMessage msg attrs
