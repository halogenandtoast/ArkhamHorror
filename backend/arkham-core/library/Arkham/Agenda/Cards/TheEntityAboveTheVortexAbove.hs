module Arkham.Agenda.Cards.TheEntityAboveTheVortexAbove
  ( TheEntityAboveTheVortexAbove
  , theEntityAboveTheVortexAbove
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Message
import Arkham.Target

newtype TheEntityAboveTheVortexAbove = TheEntityAboveTheVortexAbove AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEntityAboveTheVortexAbove :: AgendaCard TheEntityAboveTheVortexAbove
theEntityAboveTheVortexAbove = agenda (2, C) TheEntityAboveTheVortexAbove Cards.theEntityAboveTheVortexAbove (Static 6)

instance HasModifiersFor TheEntityAboveTheVortexAbove where
  getModifiersFor _ (AgendaTarget aid) (TheEntityAboveTheVortexAbove attrs)
    | aid /= toId attrs = pure $ toModifiers attrs [DoNotCountDoom]
  getModifiersFor _ _ _ = pure []

instance RunMessage TheEntityAboveTheVortexAbove where
  runMessage msg a@(TheEntityAboveTheVortexAbove attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide D attrs ->
        a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      _ -> TheEntityAboveTheVortexAbove <$> runMessage msg attrs
