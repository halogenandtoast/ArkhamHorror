module Arkham.Agenda.Cards.TheProliferationOfTheSwarm (theProliferationOfTheSwarm) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.ChaosToken
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead, getSetAsideCardsMatching)
import Arkham.Helpers.Scenario (getIsStandalone)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (pattern PlaceDoom)
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheProliferationOfTheSwarm = TheProliferationOfTheSwarm AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theProliferationOfTheSwarm :: AgendaCard TheProliferationOfTheSwarm
theProliferationOfTheSwarm =
  agenda (1, A) TheProliferationOfTheSwarm Cards.theProliferationOfTheSwarm (Static 6)

instance RunMessage TheProliferationOfTheSwarm where
  runMessage msg a@(TheProliferationOfTheSwarm attrs) = runQueueT $ case msg of
    PlaceDoom source (isTarget attrs -> True) n | agendaWards attrs > 0 -> do
      wardPlaceDoom attrs source n
      pure a
    ForTarget (isTarget attrs -> True) AdvanceAgendaIfThresholdSatisfied -> do
      factionAgendaCheckThreshold attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      whenM getIsStandalone $ addChaosToken ElderThing
      trylogogs <- getSetAsideCardsMatching $ cardIs Enemies.trylogog
      curses <- getSetAsideCardsMatching $ cardIs Treacheries.transmogrify
      shuffleCardsIntoDeck Deck.EncounterDeck (take 3 trylogogs <> curses)
      shuffleEncounterDiscardBackIn
      whenM (isFirstAgendaToAdvanceTo 1 attrs) do
        for_ (drop 3 trylogogs) \trylogog ->
          selectForMaybeM (locationIs Locations.theBurningPit) \burningPit -> do
            enemy <- createEnemyAt trylogog burningPit
            lead <- getLead
            push $ PlaceSwarmCards lead enemy 2
      advanceAgendaDeck attrs
      pure a
    _ -> TheProliferationOfTheSwarm <$> liftRunMessage msg attrs
