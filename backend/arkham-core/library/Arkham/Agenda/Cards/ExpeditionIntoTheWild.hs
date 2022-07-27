module Arkham.Agenda.Cards.ExpeditionIntoTheWild
  ( ExpeditionIntoTheWild(..)
  , expeditionIntoTheWild
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

newtype ExpeditionIntoTheWild = ExpeditionIntoTheWild AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

expeditionIntoTheWild :: AgendaCard ExpeditionIntoTheWild
expeditionIntoTheWild =
  agenda (1, A) ExpeditionIntoTheWild Cards.expeditionIntoTheWild (Static 6)

instance HasAbilities ExpeditionIntoTheWild where
  getAbilities (ExpeditionIntoTheWild a) =
    [mkAbility a 1 $ ActionAbility (Just Action.Explore) $ ActionCost 1]

instance RunMessage ExpeditionIntoTheWild where
  runMessage msg a@(ExpeditionIntoTheWild attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      -- explorationDeck <- scenarioFieldMap ScenarioDecks (findWithDefault (error "missing deck") ExplorationDeck)
      -- let
      --   (notMatched, matchedOnTop) = break (`cardMatch`
      pure a
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> ExpeditionIntoTheWild <$> runMessage msg attrs
