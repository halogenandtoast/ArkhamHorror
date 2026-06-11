module Arkham.Agenda.Cards.TheSummoningProgresses (theSummoningProgresses) where

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

newtype TheSummoningProgresses = TheSummoningProgresses AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSummoningProgresses :: AgendaCard TheSummoningProgresses
theSummoningProgresses = agenda (2, A) TheSummoningProgresses Cards.theSummoningProgresses (Static 6)

instance RunMessage TheSummoningProgresses where
  runMessage msg a@(TheSummoningProgresses attrs) = runQueueT $ case msg of
    PlaceDoom source (isTarget attrs -> True) n | agendaWards attrs > 0 -> do
      wardPlaceDoom attrs source n
      pure a
    ForTarget (isTarget attrs -> True) AdvanceAgendaIfThresholdSatisfied -> do
      factionAgendaCheckThreshold attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      whenM getIsStandalone $ addChaosToken Tablet
      whenJustM (getSetAsideCardMaybe Enemies.huneStitchedHerald) \herald ->
        shuffleCardsIntoDeck Deck.EncounterDeck [herald]
      whenM (isFirstAgendaToAdvanceTo 2 attrs) do
        whenJustM (getSetAsideCardMaybe Enemies.theInescapableMaw) \maw ->
          selectForMaybeM (locationIs Locations.athenaeumOfTheEmptySky) \athenaeum ->
            createEnemyAt_ maw athenaeum
      advanceAgendaDeck attrs
      pure a
    _ -> TheSummoningProgresses <$> liftRunMessage msg attrs
