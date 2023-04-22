module Arkham.Agenda.Cards.BehindTheCurtain
  ( BehindTheCurtain(..)
  , behindTheCurtain
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Message
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype BehindTheCurtain = BehindTheCurtain AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

behindTheCurtain :: AgendaCard BehindTheCurtain
behindTheCurtain =
  agenda (2, A) BehindTheCurtain Cards.behindTheCurtain (Static 6)

instance HasAbilities BehindTheCurtain where
  getAbilities (BehindTheCurtain attrs) =
    [mkAbility attrs 1 $ ActionAbility (Just Action.Resign) (ActionCost 1)]

instance RunMessage BehindTheCurtain where
  runMessage msg a@(BehindTheCurtain attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push (Resign iid)
      BehindTheCurtain <$> runMessage msg attrs
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      deckCount <- getActDecksInPlayCount

      pushAll
        $ [ ShuffleEncounterDiscardBackIn
          , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
          ]
        <> [ PlaceDoomOnAgenda | deckCount <= 2 ]
        <> [ PlaceDoomOnAgenda | deckCount == 1 ]
      pure a
    _ -> BehindTheCurtain <$> runMessage msg attrs
