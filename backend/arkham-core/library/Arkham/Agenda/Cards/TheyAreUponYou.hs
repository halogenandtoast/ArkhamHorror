module Arkham.Agenda.Cards.TheyAreUponYou
  ( TheyAreUponYou(..)
  , theyAreUponYou
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype TheyAreUponYou = TheyAreUponYou AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theyAreUponYou :: AgendaCard TheyAreUponYou
theyAreUponYou = agenda (3, A) TheyAreUponYou Cards.theyAreUponYou (Static 12)

instance RunMessage TheyAreUponYou where
  runMessage msg a@(TheyAreUponYou attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> TheyAreUponYou <$> runMessage msg attrs
