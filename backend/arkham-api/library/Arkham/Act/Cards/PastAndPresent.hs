module Arkham.Act.Cards.PastAndPresent (pastAndPresent) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (DefaultReplace))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Trait

newtype PastAndPresent = PastAndPresent ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pastAndPresent :: ActCard PastAndPresent
pastAndPresent = act (2, A) PastAndPresent Cards.pastAndPresent Nothing

instance HasAbilities PastAndPresent where
  getAbilities = actAbilities1 \a ->
    restricted a 1 (LocationCount 6 $ withTrait Tenochtitlan <> LocationWithoutClues)
      $ Objective
      $ forced AnyWindow

instance RunMessage PastAndPresent where
  runMessage msg a@(PastAndPresent attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      timeCollapsing <- getSetAsideCard Agendas.timeCollapsing
      theReturnTrip <- getSetAsideCard Acts.theReturnTrip
      selectEach (LocationWithTrait Tenochtitlan <> LocationWithoutClues) addToVictory
      doStep 1 msg
      eachInvestigator (discardAllClues attrs)
      doStep 2 msg
      push $ SetCurrentAgendaDeck 1 [timeCollapsing]
      push $ SetCurrentActDeck 1 [theReturnTrip]
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
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
    DoStep 2 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      padma <- getSetAsideCard Enemies.padmaAmrita
      temploMayor <- selectJust $ LocationWithTitle "Templo Mayor"
      createEnemyAt_ padma temploMayor
      pure a
    _ -> PastAndPresent <$> liftRunMessage msg attrs
