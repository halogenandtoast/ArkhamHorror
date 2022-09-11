module Arkham.Agenda.Cards.BehindTheCurtain
  ( BehindTheCurtain(..)
  , behindTheCurtain
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype BehindTheCurtain = BehindTheCurtain AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

behindTheCurtain :: AgendaCard BehindTheCurtain
behindTheCurtain =
  agenda (2, A) BehindTheCurtain Cards.behindTheCurtain (Static 12)

instance RunMessage BehindTheCurtain where
  runMessage msg a@(BehindTheCurtain attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> BehindTheCurtain <$> runMessage msg attrs
