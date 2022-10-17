module Arkham.Act.Cards.SearchForThePattern
  ( SearchForThePattern(..)
  , searchForThePattern
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message

newtype SearchForThePattern = SearchForThePattern ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

searchForThePattern :: ActCard SearchForThePattern
searchForThePattern = act
  (1, A)
  SearchForThePattern
  Cards.searchForThePattern
  (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance RunMessage SearchForThePattern where
  runMessage msg a@(SearchForThePattern attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      theWingedSerpent <- genCard Enemies.theWingedSerpent
      pushAll
        [ CreateEnemy theWingedSerpent
        , AdvanceAgendaDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> SearchForThePattern <$> runMessage msg attrs
