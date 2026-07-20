module Arkham.Agenda.Cards.AgonyAndDespair (agonyAndDespair) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Doom (getDoomCount)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher

newtype AgonyAndDespair = AgonyAndDespair AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

agonyAndDespair :: AgendaCard AgonyAndDespair
agonyAndDespair = agenda (2, A) AgonyAndDespair Cards.agonyAndDespair (Static 7)

instance RunMessage AgonyAndDespair where
  runMessage msg a@(AgonyAndDespair attrs) = runQueueT $ case msg of
    ForTarget (isTarget attrs -> True) AdvanceAgendaIfThresholdSatisfied -> do
      n <- getDoomCount
      when (n >= 11) $ scenarioSpecific_ "doomThresholdMet"
      AgonyAndDespair <$> liftRunMessage msg attrs
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      actId <- selectJust AnyAct
      push $ AdvanceAct actId (toSource lead) #other
      advanceAgendaDeck attrs
      pure a
    _ -> AgonyAndDespair <$> liftRunMessage msg attrs
