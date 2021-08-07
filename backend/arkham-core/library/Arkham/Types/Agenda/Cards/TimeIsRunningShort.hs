module Arkham.Types.Agenda.Cards.TimeIsRunningShort
  ( TimeIsRunningShort(..)
  , timeIsRunningShort
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.Source
import Arkham.Types.Window

newtype TimeIsRunningShort = TimeIsRunningShort AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeIsRunningShort :: AgendaCard TimeIsRunningShort
timeIsRunningShort =
  agenda (2, A) TimeIsRunningShort Cards.timeIsRunningShort (Static 8)

instance HasModifiersFor env TimeIsRunningShort

instance HasActions env TimeIsRunningShort where
  getActions _ NonFast (TimeIsRunningShort a) =
    pure [mkAbility a 1 $ ActionAbility (Just Action.Resign) (ActionCost 1)]
  getActions _ _ _ = pure []

instance (AgendaRunner env) => RunMessage env TimeIsRunningShort where
  runMessage msg a@(TimeIsRunningShort attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B ->
      a <$ push (ScenarioResolution $ Resolution 2)
    UseCardAbility iid (AgendaSource aid) _ 1 _ | aid == agendaId -> do
      push (Resign iid)
      TimeIsRunningShort <$> runMessage msg attrs
    _ -> TimeIsRunningShort <$> runMessage msg attrs
