module Arkham.Types.Agenda.Cards.LockedInside
  ( LockedInside
  , lockedInside
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message

newtype LockedInside = LockedInside AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedInside :: AgendaCard LockedInside
lockedInside = agenda (1, A) LockedInside Cards.lockedInside (Static 2)

instance AgendaRunner env => RunMessage env LockedInside where
  runMessage msg a@(LockedInside attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> LockedInside <$> runMessage msg attrs
