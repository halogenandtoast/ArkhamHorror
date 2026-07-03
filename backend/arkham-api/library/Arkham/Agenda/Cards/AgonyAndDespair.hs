module Arkham.Agenda.Cards.AgonyAndDespair (agonyAndDespair) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Doom (getDoomCount)
import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher

newtype AgonyAndDespair = AgonyAndDespair AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

agonyAndDespair :: AgendaCard AgonyAndDespair
agonyAndDespair = agenda (2, A) AgonyAndDespair Cards.agonyAndDespair (Static 7)

-- In Single Group Mode, if the investigators feel that they have completed
-- their objective with time to spare, they may add doom to the agenda until
-- its doom threshold is satisfied during the mythos phase.
instance HasAbilities AgonyAndDespair where
  getAbilities (AgonyAndDespair a) =
    guard (onSide A a) *> [restricted a 1 (DuringPhase #mythos) (FastAbility Free)]

instance RunMessage AgonyAndDespair where
  runMessage msg a@(AgonyAndDespair attrs) = runQueueT $ case msg of
    ForTarget (isTarget attrs -> True) AdvanceAgendaIfThresholdSatisfied -> do
      n <- getDoomCount
      when (n >= 11) $ scenarioSpecific_ "doomThresholdMet"
      AgonyAndDespair <$> liftRunMessage msg attrs
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      let mThreshold = agendaDoomThreshold attrs
      for_ mThreshold \gv -> do
        threshold <- getGameValue gv
        doom <- getDoomCount
        when (threshold > doom) $ placeDoom (attrs.ability 1) attrs (threshold - doom)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      actId <- selectJust AnyAct
      push $ AdvanceAct actId (toSource lead) #other
      advanceAgendaDeck attrs
      pure a
    _ -> AgonyAndDespair <$> liftRunMessage msg attrs
