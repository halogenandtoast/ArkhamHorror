module Arkham.Agenda.Cards.TimeIsRunningShort (
  TimeIsRunningShort (..),
  timeIsRunningShort,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue

newtype TimeIsRunningShort = TimeIsRunningShort AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

timeIsRunningShort :: AgendaCard TimeIsRunningShort
timeIsRunningShort =
  agenda (2, A) TimeIsRunningShort Cards.timeIsRunningShort (Static 8)

instance HasAbilities TimeIsRunningShort where
  getAbilities (TimeIsRunningShort a) =
    [mkAbility a 1 $ ActionAbility [Action.Resign] (ActionCost 1)]

instance RunMessage TimeIsRunningShort where
  runMessage msg a@(TimeIsRunningShort attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      push R2
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ Resign iid
      pure a
    _ -> TimeIsRunningShort <$> runMessage msg attrs
