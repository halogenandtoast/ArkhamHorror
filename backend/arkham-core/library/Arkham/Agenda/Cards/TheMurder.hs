module Arkham.Agenda.Cards.TheMurder (
  TheMurder (..),
  theMurder,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Placement
import Arkham.ScenarioLogKey
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheMurder = TheMurder AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theMurder :: AgendaCard TheMurder
theMurder = agenda (1, A) TheMurder Cards.theMurder (Static 3)

instance HasModifiersFor TheMurder where
  getModifiersFor target (TheMurder a) | a `is` target = do
    n <- getPlayerCount
    pure $ toModifiers a [DoomThresholdModifier 1 | n == 1]
  getModifiersFor _ _ = pure []

instance RunMessage TheMurder where
  runMessage msg a@(TheMurder attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        foyer <- getJustLocationByName "Foyer"
        secondFloorHall <- getJustLocationByName "Second Floor Hall"
        sergeantMonroe <- getSetAsideCard Assets.sergeantMonroe
        (officer1, officer2, officer3) <-
          getSetAsideCardsMatching (cardIs Enemies.arkhamOfficer) <&> \case
            [x, y, z] -> (x, y, z)
            _ -> error "could not find correct number of arkham officers"

        n <- getPlayerCount
        assetId <- getRandom

        createOfficer1 <- createEnemyAt_ officer1 secondFloorHall Nothing
        createOfficer2 <- createEnemyAt_ officer2 foyer Nothing

        lead <- getLead
        whatHaveYouDone <- getSetAsideCard Treacheries.whatHaveYouDone

        pushAll
          $ [CreateAssetAt assetId sergeantMonroe (AtLocation foyer), createOfficer1]
          <> [createOfficer2 | n > 2]
          <> [ ShuffleCardsIntoDeck Deck.EncounterDeck (officer3 : [officer2 | n < 3])
             , ShuffleEncounterDiscardBackIn
             , DrewTreachery lead Nothing whatHaveYouDone
             , NextAdvanceAgendaStep (toId attrs) 1
             , advanceAgendaDeck attrs
             ]
        pure a
      NextAdvanceAgendaStep aid 1 | aid == toId attrs -> do
        room225 <- getJustLocationByName "Room 225"
        x <- count id <$> traverse (fmap not . remembered) [CleanedUpTheBlood, HidTheBody, TidiedUpTheRoom]
        arkhamOfficers <- selectList $ enemyIs Enemies.arkhamOfficer
        lead <- getLeadPlayer
        pushAll
          $ [PlaceClues (toSource aid) (toTarget room225) x]
          <> replicate
            x
            ( chooseOrRunOne
                lead
                [targetLabel officer [PlaceDoom (toSource aid) (toTarget officer) 1] | officer <- arkhamOfficers]
            )
        pure a
      _ -> TheMurder <$> runMessage msg attrs
