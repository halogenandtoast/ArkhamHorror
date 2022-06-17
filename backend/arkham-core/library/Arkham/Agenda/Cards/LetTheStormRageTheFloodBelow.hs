module Arkham.Agenda.Cards.LetTheStormRageTheFloodBelow
  ( LetTheStormRageTheFloodBelow
  , letTheStormRageTheFloodBelow
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message

newtype LetTheStormRageTheFloodBelow = LetTheStormRageTheFloodBelow AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

letTheStormRageTheFloodBelow :: AgendaCard LetTheStormRageTheFloodBelow
letTheStormRageTheFloodBelow = agenda
  (2, A)
  LetTheStormRageTheFloodBelow
  Cards.letTheStormRageTheFloodBelow
  (Static 6)

instance HasModifiersFor LetTheStormRageTheFloodBelow where
  getModifiersFor _ (AgendaTarget aid) (LetTheStormRageTheFloodBelow attrs)
    | aid /= toId attrs = pure $ toModifiers attrs [DoNotCountDoom]
  getModifiersFor _ _ _ = pure []

instance RunMessage LetTheStormRageTheFloodBelow where
  runMessage msg a@(LetTheStormRageTheFloodBelow attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> LetTheStormRageTheFloodBelow <$> runMessage msg attrs
