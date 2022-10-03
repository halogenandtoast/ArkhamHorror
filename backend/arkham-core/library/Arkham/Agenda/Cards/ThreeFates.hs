module Arkham.Agenda.Cards.ThreeFates
  ( ThreeFates(..)
  , threeFates
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Cost
import Arkham.GameValue
import Arkham.Message
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype ThreeFates = ThreeFates AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

threeFates :: AgendaCard ThreeFates
threeFates = agenda (1, A) ThreeFates Cards.threeFates (Static 6)

instance HasAbilities ThreeFates where
  getAbilities (ThreeFates attrs) =
    [mkAbility attrs 1 $ ActionAbility (Just Action.Resign) (ActionCost 1)]

instance RunMessage ThreeFates where
  runMessage msg a@(ThreeFates attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push (Resign iid)
      ThreeFates <$> runMessage msg attrs
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      deckCount <- getActDecksInPlayCount

      pushAll
        $ [ ShuffleEncounterDiscardBackIn
          , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
          ]
        <> [ PlaceDoomOnAgenda | deckCount == 2 ]
      pure a
    _ -> ThreeFates <$> runMessage msg attrs
