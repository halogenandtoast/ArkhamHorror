module Arkham.Agenda.Cards.TheHangedManXII (theHangedManXII) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Trait (Trait (Spectral))
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheHangedManXII = TheHangedManXII AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHangedManXII :: AgendaCard TheHangedManXII
theHangedManXII = agenda (1, A) TheHangedManXII Cards.theHangedManXII (Static 8)

instance RunMessage TheHangedManXII where
  runMessage msg a@(TheHangedManXII attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      flippableLocations <- select $ LocationWithoutModifier CannotBeFlipped <> not_ (withTrait Spectral)
      for_ flippableLocations (flipOverBy lead attrs)

      spectralWatcher <- getSetAsideCard Enemies.theSpectralWatcher
      hangmansBrook <- getJustLocationByName "Hangman's Brook"
      createEnemyAt_ spectralWatcher hangmansBrook

      watchersGrasps <- getSetAsideCardsMatching $ cardIs Treacheries.watchersGrasp
      spectralDiscards <- getSpectralDiscards
      shuffleCardsIntoDeck SpectralEncounterDeck (watchersGrasps <> map EncounterCard spectralDiscards)
      advanceAgendaDeck attrs
      pure a
    _ -> TheHangedManXII <$> liftRunMessage msg attrs
