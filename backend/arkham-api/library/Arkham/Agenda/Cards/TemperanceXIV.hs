module Arkham.Agenda.Cards.TemperanceXIV (temperanceXIV) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait (Trait (Witch))

newtype TemperanceXIV = TemperanceXIV AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temperanceXIV :: AgendaCard TemperanceXIV
temperanceXIV = agenda (1, A) TemperanceXIV Cards.temperanceXIV (Static 8)

instance RunMessage TemperanceXIV where
  runMessage msg a@(TemperanceXIV attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      encounterDeck <- scenarioFieldMap ScenarioEncounterDeck unDeck
      let discardAmount = max 0 (length encounterDeck - 5)
      lead <- getLead
      discardTopOfEncounterDeckAndHandle lead attrs discardAmount attrs
      advanceAgendaDeck attrs
      pure a
    DiscardedTopOfEncounterDeck _ _ _ (isTarget attrs -> True) -> do
      mWitch <- find (`cardMatch` CardWithTrait Witch) <$> scenarioField ScenarioDiscard
      for_ mWitch \witch -> do
        investigators <- select InvestigatorWithMostCardsInPlayArea
        leadChooseOrRunOneM $ targets investigators (`drawCard` witch)
      pure a
    _ -> TemperanceXIV <$> liftRunMessage msg attrs
