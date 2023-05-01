module Arkham.Agenda.Cards.TheHangedManXII
  ( TheHangedManXII(..)
  , theHangedManXII
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheHangedManXII = TheHangedManXII AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHangedManXII :: AgendaCard TheHangedManXII
theHangedManXII = agenda (1, A) TheHangedManXII Cards.theHangedManXII (Static 8)

instance RunMessage TheHangedManXII where
  runMessage msg a@(TheHangedManXII attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheHangedManXII <$> runMessage msg attrs
