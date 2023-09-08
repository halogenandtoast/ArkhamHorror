module Arkham.Agenda.Cards.WheelOfFortuneX
  ( WheelOfFortuneX(..)
  , wheelOfFortuneX
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype WheelOfFortuneX = WheelOfFortuneX AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wheelOfFortuneX :: AgendaCard WheelOfFortuneX
wheelOfFortuneX = agenda (1, A) WheelOfFortuneX Cards.wheelOfFortuneX (Static 4)

instance RunMessage WheelOfFortuneX where
  runMessage msg a@(WheelOfFortuneX attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
        a <$ pushAll [advanceAgendaDeck attrs]
      _ -> WheelOfFortuneX <$> runMessage msg attrs
