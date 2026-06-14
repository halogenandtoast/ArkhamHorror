module Arkham.Agenda.Cards.RuinedArchives (ruinedArchives) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait (Trait (Glyph, Omen))

newtype RuinedArchives = RuinedArchives AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinedArchives :: AgendaCard RuinedArchives
ruinedArchives = agenda (1, A) RuinedArchives Cards.ruinedArchives (Static 6)

instance RunMessage RuinedArchives where
  runMessage msg a@(RuinedArchives attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      cards <-
        filterCards (CardWithOneOf [CardWithTrait Omen, CardWithTrait Glyph])
          <$> scenarioField ScenarioDiscard
      push $ ShuffleCardsIntoDeck Deck.EncounterDeck (map toCard cards)
      advanceAgendaDeck attrs
      pure a
    _ -> RuinedArchives <$> liftRunMessage msg attrs
