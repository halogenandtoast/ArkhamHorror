module Arkham.Agenda.Cards.TheIncubationOfTheEgg (theIncubationOfTheEgg) where

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

newtype TheIncubationOfTheEgg = TheIncubationOfTheEgg AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theIncubationOfTheEgg :: AgendaCard TheIncubationOfTheEgg
theIncubationOfTheEgg = agenda (1, A) TheIncubationOfTheEgg Cards.theIncubationOfTheEgg (Static 6)

instance RunMessage TheIncubationOfTheEgg where
  runMessage msg a@(TheIncubationOfTheEgg attrs) = runQueueT $ case msg of
    PlaceDoom source (isTarget attrs -> True) n | agendaWards attrs > 0 -> do
      wardPlaceDoom attrs source n
      pure a
    ForTarget (isTarget attrs -> True) AdvanceAgendaIfThresholdSatisfied -> do
      factionAgendaCheckThreshold attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      whenM getIsStandalone $ addChaosToken Cultist
      bringers <- getSetAsideCardsMatching $ cardIs Enemies.bringerOfParadise
      hazards <- getSetAsideCardsMatching $ cardIs Treacheries.huntDown
      shuffleCardsIntoDeck Deck.EncounterDeck (bringers <> hazards)
      shuffleEncounterDiscardBackIn
      whenM (isFirstAgendaToAdvanceTo 1 attrs) do
        whenJustM (getSetAsideCardMaybe Enemies.horrificShoggoth) \shoggoth ->
          selectForMaybeM (locationIs Locations.shrineOfMaghanArkat) \shrine ->
            createEnemyAt_ shoggoth shrine
      advanceAgendaDeck attrs
      pure a
    _ -> TheIncubationOfTheEgg <$> liftRunMessage msg attrs
