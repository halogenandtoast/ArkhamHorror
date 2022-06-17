module Arkham.Agenda.Cards.LetTheStormRageTheVortexAbove
  ( LetTheStormRageTheVortexAbove
  , letTheStormRageTheVortexAbove
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype LetTheStormRageTheVortexAbove = LetTheStormRageTheVortexAbove AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

letTheStormRageTheVortexAbove :: AgendaCard LetTheStormRageTheVortexAbove
letTheStormRageTheVortexAbove = agenda
  (2, A)
  LetTheStormRageTheVortexAbove
  Cards.letTheStormRageTheVortexAbove
  (Static 6)

instance HasModifiersFor LetTheStormRageTheVortexAbove where
  getModifiersFor _ (AgendaTarget aid) (LetTheStormRageTheVortexAbove attrs)
    | aid /= toId attrs = pure $ toModifiers attrs [DoNotCountDoom]
  getModifiersFor _ _ _ = pure []

instance RunMessage LetTheStormRageTheVortexAbove where
  runMessage msg a@(LetTheStormRageTheVortexAbove attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> LetTheStormRageTheVortexAbove <$> runMessage msg attrs
