module Arkham.Agenda.Cards.TheProliferationProgresses (theProliferationProgresses) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.ChaosToken
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCardMaybe)
import Arkham.Helpers.Scenario (getIsStandalone)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (pattern PlaceDoom)
import Arkham.Scenarios.WarOfTheOuterGods.Helpers

newtype TheProliferationProgresses = TheProliferationProgresses AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theProliferationProgresses :: AgendaCard TheProliferationProgresses
theProliferationProgresses =
  agenda (2, A) TheProliferationProgresses Cards.theProliferationProgresses (Static 6)

instance RunMessage TheProliferationProgresses where
  runMessage msg a@(TheProliferationProgresses attrs) = runQueueT $ case msg of
    PlaceDoom source (isTarget attrs -> True) n | agendaWards attrs > 0 -> do
      wardPlaceDoom attrs source n
      pure a
    ForTarget (isTarget attrs -> True) AdvanceAgendaIfThresholdSatisfied -> do
      factionAgendaCheckThreshold attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      whenM getIsStandalone $ addChaosToken ElderThing
      whenJustM (getSetAsideCardMaybe Enemies.trylogog) \trylogog ->
        shuffleCardsIntoDeck Deck.EncounterDeck [trylogog]
      whenM (isFirstAgendaToAdvanceTo 2 attrs) do
        whenJustM (getSetAsideCardMaybe Enemies.droningHorde) \horde ->
          selectForMaybeM (locationIs Locations.theBurningPit) \burningPit ->
            createEnemyAt_ horde burningPit
      advanceAgendaDeck attrs
      pure a
    _ -> TheProliferationProgresses <$> liftRunMessage msg attrs
