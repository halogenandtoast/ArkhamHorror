module Arkham.Agenda.Cards.TheArkhamWoods (theArkhamWoods) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Trait

newtype TheArkhamWoods = TheArkhamWoods AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theArkhamWoods :: AgendaCard TheArkhamWoods
theArkhamWoods = agenda (1, A) TheArkhamWoods Cards.theArkhamWoods (Static 4)

instance RunMessage TheArkhamWoods where
  runMessage msg a@(TheArkhamWoods attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      lead <- getLead
      discardUntilFirst lead attrs Deck.EncounterDeck $ basic $ #enemy <> CardWithTrait Monster
      advanceAgendaDeck attrs
      pure a
    RequestedEncounterCard (isSource attrs -> True) _ (Just card) -> do
      mainPath <- getJustLocationByName "Main Path"
      enemy <- createEnemyAt card mainPath
      placeDoom attrs enemy 1
      pure a
    _ -> TheArkhamWoods <$> liftRunMessage msg attrs
