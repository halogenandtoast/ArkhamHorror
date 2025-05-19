module Arkham.Agenda.Cards.TheShapeOfChaos (theShapeOfChaos) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection

newtype TheShapeOfChaos = TheShapeOfChaos AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theShapeOfChaos :: AgendaCard TheShapeOfChaos
theShapeOfChaos = agenda (2, A) TheShapeOfChaos Cards.theShapeOfChaos (Static 5)

instance RunMessage TheShapeOfChaos where
  runMessage msg a@(TheShapeOfChaos attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      n <- getCurrentActStep
      when (n == 4) do
        towers <- select $ LocationWithTitle "Forsaken Tower"
        for_ towers \tower -> do
          x <- fieldMapM LocationRevealClues getGameValue tower
          push $ PlaceCluesUpToClueValue tower (toSource attrs) x

        nyarlathoteps <- selectWithField EnemyPlacement $ EnemyWithTitle "Nyarlathotep"

        for_ nyarlathoteps \(nyarlathotep, p) -> do
          case p of
            HiddenInHand _ -> do
              card <- field EnemyCard nyarlathotep
              push $ RevealCard (toCardId card)
            _ -> pure ()

        let investigatorsWithNyarlathotep = [iid | (_, HiddenInHand iid) <- nyarlathoteps]
        selectEach (not_ $ mapOneOf InvestigatorWithId investigatorsWithNyarlathotep) \iid -> do
          chooseOneM iid do
            labeled "Take 1 Damage" $ assignDamage iid attrs 1
            labeled "Take 1 Horror" $ assignHorror iid attrs 1

        for_ nyarlathoteps \(nyarlathotep, p) -> do
          case p of
            HiddenInHand iid -> do
              card <- field EnemyCard nyarlathotep
              focusCard card do
                chooseOneM iid do
                  questionLabeled "Choose:"
                  questionLabeledCard card
                  labeled "Nyarlathotep immediately attacks you and is shuffled into the encounter deck." do
                    unfocusCards
                    initiateEnemyAttack nyarlathotep attrs iid
                    shuffleBackIntoEncounterDeck nyarlathotep
                  labeled
                    "Nyarlathotep immediately attacks you three times and is returned to that investigator's hand."
                    do
                      unfocusCards
                      initiateEnemyAttack nyarlathotep attrs iid
                      initiateEnemyAttack nyarlathotep attrs iid
                      initiateEnemyAttack nyarlathotep attrs iid
            _ -> pure ()

      advanceAgendaDeck attrs
      pure a
    _ -> TheShapeOfChaos <$> liftRunMessage msg attrs
