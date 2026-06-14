module Arkham.Agenda.Cards.StirringInTheDark (stirringInTheDark) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead, getSetAsideCard)
import Arkham.Helpers.Scenario (getEncounterDiscard)
import Arkham.Matcher
import Arkham.Scenario.Deck (ScenarioEncounterDeckKey (..))

newtype StirringInTheDark = StirringInTheDark AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stirringInTheDark :: AgendaCard StirringInTheDark
stirringInTheDark = agenda (1, A) StirringInTheDark Cards.stirringInTheDark (Static 12)

instance RunMessage StirringInTheDark where
  runMessage msg a@(StirringInTheDark attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- Place clues on each location without Victory X up to its clue value.
      locations <- select $ not_ LocationWithVictory
      for_ locations $ placeCluesUpToClueValue attrs

      -- Shuffle the set-aside Grotesque Amalgam with the encounter discard pile
      -- and place those cards on the bottom of the encounter deck.
      lead <- getLead
      grotesqueAmalgam <- getSetAsideCard Enemies.grotesqueAmalgam
      discardPile <- getEncounterDiscard RegularEncounterDeck
      shuffled <- shuffleM (toCard grotesqueAmalgam : map toCard discardPile)
      pushAll $ map RemoveFromEncounterDiscard discardPile
      for_ shuffled $ putCardOnBottomOfDeck lead Deck.EncounterDeck

      advanceAgendaDeck attrs
      pure a
    _ -> StirringInTheDark <$> liftRunMessage msg attrs
