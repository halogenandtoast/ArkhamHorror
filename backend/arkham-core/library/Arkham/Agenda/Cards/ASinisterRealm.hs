module Arkham.Agenda.Cards.ASinisterRealm
  ( ASinisterRealm(..)
  , aSinisterRealm
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype ASinisterRealm = ASinisterRealm AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aSinisterRealm :: AgendaCard ASinisterRealm
aSinisterRealm = agenda (1, A) ASinisterRealm Cards.aSinisterRealm (Static 12)

instance RunMessage ASinisterRealm where
  runMessage msg a@(ASinisterRealm attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> ASinisterRealm <$> runMessage msg attrs
