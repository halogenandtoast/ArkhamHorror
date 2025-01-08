module Arkham.Agenda.Cards.RiseOfTheGhouls (riseOfTheGhouls) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.Matcher
import Arkham.Trait

newtype RiseOfTheGhouls = RiseOfTheGhouls AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riseOfTheGhouls :: AgendaCard RiseOfTheGhouls
riseOfTheGhouls = agenda (2, A) RiseOfTheGhouls Cards.riseOfTheGhouls (Static 7)

instance RunMessage RiseOfTheGhouls where
  runMessage msg a@(RiseOfTheGhouls attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      lead <- getLead
      discardUntilFirst lead attrs Deck.EncounterDeck (basic $ #enemy <> withTrait Ghoul)
      pure a
    RequestedEncounterCard (isSource attrs -> True) (Just iid) mcard -> do
      for_ mcard (drawCard iid)
      advanceAgendaDeck attrs
      pure a
    _ -> RiseOfTheGhouls <$> liftRunMessage msg attrs
