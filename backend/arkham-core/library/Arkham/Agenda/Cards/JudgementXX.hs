module Arkham.Agenda.Cards.JudgementXX
  ( JudgementXX(..)
  , judgementXX
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype JudgementXX = JudgementXX AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

judgementXX :: AgendaCard JudgementXX
judgementXX = agenda (1, A) JudgementXX Cards.judgementXX (Static 12)

instance RunMessage JudgementXX where
  runMessage msg a@(JudgementXX attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      _ -> JudgementXX <$> runMessage msg attrs
