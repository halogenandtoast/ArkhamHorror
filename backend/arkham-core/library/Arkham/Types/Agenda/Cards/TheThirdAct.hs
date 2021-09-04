module Arkham.Types.Agenda.Cards.TheThirdAct
  ( TheThirdAct
  , theThirdAct
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message

newtype TheThirdAct = TheThirdAct AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThirdAct :: AgendaCard TheThirdAct
theThirdAct = agenda (1, A) TheThirdAct Cards.theThirdAct (Static 6)

instance AgendaRunner env => RunMessage env TheThirdAct where
  runMessage msg a@(TheThirdAct attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B ->
      a <$ pushAll [NextAgenda aid "TODO"]
    _ -> TheThirdAct <$> runMessage msg attrs
