module Arkham.Agenda.Cards.TimeIsRunningShort (timeIsRunningShort) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted

newtype TimeIsRunningShort = TimeIsRunningShort AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeIsRunningShort :: AgendaCard TimeIsRunningShort
timeIsRunningShort = agenda (2, A) TimeIsRunningShort Cards.timeIsRunningShort (Static 8)

instance HasAbilities TimeIsRunningShort where
  getAbilities (TimeIsRunningShort a) = [mkAbility a 1 $ ActionAbility [Action.Resign] (ActionCost 1)]

instance RunMessage TimeIsRunningShort where
  runMessage msg a@(TimeIsRunningShort attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R2
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resign iid
      pure a
    _ -> TimeIsRunningShort <$> liftRunMessage msg attrs
