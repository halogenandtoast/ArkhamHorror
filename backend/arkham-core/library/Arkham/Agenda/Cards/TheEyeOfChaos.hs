module Arkham.Agenda.Cards.TheEyeOfChaos (TheEyeOfChaos (..), theEyeOfChaos) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Attack
import Arkham.Card
import Arkham.Enemy.Types (Field (..))
import Arkham.Game.Helpers (getGameValue, getPlayer)
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message (questionLabelWithCard)
import Arkham.Placement
import Arkham.Projection

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
        towers <- select $ LocationWithTitle "Forsaken Tower"
        for_ towers \tower -> do
          x <- fieldMapM LocationRevealClues getGameValue tower
          push $ PlaceCluesUpToClueValue tower (toSource attrs) x

        nyarlathoteps <- selectWithField EnemyPlacement $ EnemyWithTitle "Nyarlathotep"

        let
          investigatorsWithNyarlathotep = flip mapMaybe nyarlathoteps \(_, p) -> case p of
            StillInHand iid -> Just iid
            _ -> Nothing

        for_ nyarlathoteps \(nyarlathotep, p) -> do
          case p of
            StillInHand _ -> do
              card <- field EnemyCard nyarlathotep
              push $ RevealCard (toCardId card)
            _ -> pure ()

        selectEach (not_ $ oneOf (map InvestigatorWithId investigatorsWithNyarlathotep))
          $ \iid -> do
            chooseOne
              iid
              [ Label "Take 1 Damage" [Msg.assignDamage iid attrs 1]
              , Label "Take 1 Horror" [Msg.assignHorror iid attrs 1]
              ]

        for_ nyarlathoteps \(nyarlathotep, p) -> do
          case p of
            StillInHand iid -> do
              card <- field EnemyCard nyarlathotep
              push $ FocusCards [card]
              player <- getPlayer iid
              push
                $ questionLabelWithCard "Choose:" (toCardCode card) player
                $ ChooseOne
                  [ Label
                      "Nyarlathotep immediately attacks you and is shuffled into the encounter deck."
                      [ InitiateEnemyAttack $ enemyAttack nyarlathotep attrs iid
                      , ShuffleBackIntoEncounterDeck (toTarget nyarlathotep)
                      ]
                  , Label
                      "Nyarlathotep immediately attacks you three times and is returned to that investigator's hand."
                      [ InitiateEnemyAttack $ enemyAttack nyarlathotep attrs iid
                      , InitiateEnemyAttack $ enemyAttack nyarlathotep attrs iid
                      , InitiateEnemyAttack $ enemyAttack nyarlathotep attrs iid
                      ]
                  ]
            _ -> pure ()

      advanceAgendaDeck attrs
      pure a
    _ -> TheEyeOfChaos <$> lift (runMessage msg attrs)
