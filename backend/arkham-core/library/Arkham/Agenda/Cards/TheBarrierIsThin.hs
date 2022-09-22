module Arkham.Agenda.Cards.TheBarrierIsThin
  ( TheBarrierIsThin(..)
  , theBarrierIsThin
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheBarrierIsThin = TheBarrierIsThin AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBarrierIsThin :: AgendaCard TheBarrierIsThin
theBarrierIsThin = agenda (2, A) TheBarrierIsThin Cards.theBarrierIsThin (Static 5)

instance RunMessage TheBarrierIsThin where
  runMessage msg a@(TheBarrierIsThin attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      _ -> TheBarrierIsThin <$> runMessage msg attrs
