module Arkham.Agenda.Cards.TheLabyrinthsOfLunacy.Awakening (awakening) where

import Arkham.Agenda.CardDefs.TheLabyrinthsOfLunacy qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher

newtype Awakening = Awakening AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

awakening :: AgendaCard Awakening
awakening =
  agenda (1, A) Awakening Cards.awakening (Static 6)

instance RunMessage Awakening where
  runMessage msg a@(Awakening attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      actId <- selectJust AnyAct
      push $ AdvanceAct actId (toSource lead) #other
      advanceAgendaDeck attrs
      pure a
    _ -> Awakening <$> liftRunMessage msg attrs
