module Arkham.Agenda.Cards.BesetByMonsters
  ( BesetByMonsters(..)
  , besetByMonsters
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype BesetByMonsters = BesetByMonsters AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

besetByMonsters :: AgendaCard BesetByMonsters
besetByMonsters = agenda (2, A) BesetByMonsters Cards.besetByMonsters (Static 12)

instance RunMessage BesetByMonsters where
  runMessage msg a@(BesetByMonsters attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> BesetByMonsters <$> runMessage msg attrs
