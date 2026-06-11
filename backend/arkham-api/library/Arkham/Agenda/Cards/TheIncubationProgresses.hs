module Arkham.Agenda.Cards.TheIncubationProgresses (theIncubationProgresses) where

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

newtype TheIncubationProgresses = TheIncubationProgresses AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theIncubationProgresses :: AgendaCard TheIncubationProgresses
theIncubationProgresses = agenda (2, A) TheIncubationProgresses Cards.theIncubationProgresses (Static 6)

instance RunMessage TheIncubationProgresses where
  runMessage msg a@(TheIncubationProgresses attrs) = runQueueT $ case msg of
    PlaceDoom source (isTarget attrs -> True) n | agendaWards attrs > 0 -> do
      wardPlaceDoom attrs source n
      pure a
    ForTarget (isTarget attrs -> True) AdvanceAgendaIfThresholdSatisfied -> do
      factionAgendaCheckThreshold attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      whenM getIsStandalone $ addChaosToken Cultist
      whenJustM (getSetAsideCardMaybe Enemies.horrificShoggoth) \shoggoth ->
        shuffleCardsIntoDeck Deck.EncounterDeck [shoggoth]
      whenM (isFirstAgendaToAdvanceTo 2 attrs) do
        whenJustM (getSetAsideCardMaybe Enemies.vileBroodmaster) \broodmaster ->
          selectForMaybeM (locationIs Locations.shrineOfMaghanArkat) \shrine ->
            createEnemyAt_ broodmaster shrine
      advanceAgendaDeck attrs
      pure a
    _ -> TheIncubationProgresses <$> liftRunMessage msg attrs
