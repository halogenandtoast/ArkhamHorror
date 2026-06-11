module Arkham.Agenda.Cards.TheSummoningOfSilenus (theSummoningOfSilenus) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.ChaosToken
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCardMaybe, getSetAsideCardsMatching)
import Arkham.Helpers.Scenario (getIsStandalone)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (pattern PlaceDoom)
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheSummoningOfSilenus = TheSummoningOfSilenus AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSummoningOfSilenus :: AgendaCard TheSummoningOfSilenus
theSummoningOfSilenus = agenda (1, A) TheSummoningOfSilenus Cards.theSummoningOfSilenus (Static 6)

instance RunMessage TheSummoningOfSilenus where
  runMessage msg a@(TheSummoningOfSilenus attrs) = runQueueT $ case msg of
    PlaceDoom source (isTarget attrs -> True) n | agendaWards attrs > 0 -> do
      wardPlaceDoom attrs source n
      pure a
    ForTarget (isTarget attrs -> True) AdvanceAgendaIfThresholdSatisfied -> do
      factionAgendaCheckThreshold attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      whenM getIsStandalone $ addChaosToken Tablet
      entities <- getSetAsideCardsMatching $ cardIs Enemies.etherealEntity
      hexes <- getSetAsideCardsMatching $ cardIs Treacheries.inevitableEnd
      shuffleCardsIntoDeck Deck.EncounterDeck (entities <> hexes)
      shuffleEncounterDiscardBackIn
      whenM (isFirstAgendaToAdvanceTo 1 attrs) do
        whenJustM (getSetAsideCardMaybe Enemies.huneStitchedHerald) \herald ->
          selectForMaybeM (locationIs Locations.athenaeumOfTheEmptySky) \athenaeum ->
            createEnemyAt_ herald athenaeum
      advanceAgendaDeck attrs
      pure a
    _ -> TheSummoningOfSilenus <$> liftRunMessage msg attrs
