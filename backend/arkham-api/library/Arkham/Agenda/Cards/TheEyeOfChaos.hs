module Arkham.Agenda.Cards.TheEyeOfChaos (theEyeOfChaos) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Location.Types (Field (..))
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenarios.WhereTheGodsDwell.Helpers

newtype TheEyeOfChaos = TheEyeOfChaos AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEyeOfChaos :: AgendaCard TheEyeOfChaos
theEyeOfChaos = agenda (1, A) TheEyeOfChaos Cards.theEyeOfChaos (Static 7)

instance RunMessage TheEyeOfChaos where
  runMessage msg a@(TheEyeOfChaos attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      n <- getCurrentActStep
      when (n == 4) do
        towers <- select $ LocationWithTitle "Forsaken Tower" <> RevealedLocation
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
          chooseOneM iid $ withI18n $ countVar 1 do
            labeled' "takeDamage" $ assignDamage iid attrs 1
            labeled' "takeHorror" $ assignHorror iid attrs 1

        for_ nyarlathoteps \(nyarlathotep, p) -> do
          case p of
            HiddenInHand iid -> do
              card <- field EnemyCard nyarlathotep
              push $ FocusCards [card]
              chooseOneM iid $ scenarioI18n $ scope "theEyeOfChaos" do
                questionLabeled' "choose"
                questionLabeledCard card
                labeled' "nyarlathotepAttackOnce" do
                  initiateEnemyAttack nyarlathotep attrs iid
                  shuffleBackIntoEncounterDeck nyarlathotep
                labeled' "nyarlathotepAttackThrice" do
                    initiateEnemyAttack nyarlathotep attrs iid
                    initiateEnemyAttack nyarlathotep attrs iid
                    initiateEnemyAttack nyarlathotep attrs iid
            _ -> pure ()

      advanceAgendaDeck attrs
      pure a
    _ -> TheEyeOfChaos <$> liftRunMessage msg attrs
