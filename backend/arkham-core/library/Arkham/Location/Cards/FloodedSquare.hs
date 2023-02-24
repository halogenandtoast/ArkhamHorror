module Arkham.Location.Cards.FloodedSquare
  ( floodedSquare
  , FloodedSquare(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding ( EnemyEvaded )
import Arkham.Message
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers

newtype FloodedSquare = FloodedSquare LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

floodedSquare :: LocationCard FloodedSquare
floodedSquare = locationWith
  FloodedSquare
  Cards.floodedSquare
  4
  (PerPlayer 1)
  (connectsToL .~ singleton RightOf)

instance HasAbilities FloodedSquare where
  getAbilities (FloodedSquare attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility
            attrs
            1
            (Here <> EnemyCriteria
              (EnemyExists $ NonEliteEnemy <> EnemyAt
                (LocationInDirection RightOf $ LocationWithId $ toId attrs)
              )
            )
          $ ActionAbility Nothing
          $ ActionCost 1
        | locationRevealed attrs
        ]

instance RunMessage FloodedSquare where
  runMessage msg l@(FloodedSquare attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      counterClockwiseLocation <- getCounterClockwiseLocation (toId attrs)
      nonEliteEnemies <- selectList $ NonEliteEnemy <> EnemyAt
        (LocationWithId counterClockwiseLocation)
      l <$ push
        (chooseOne
          iid
          [ TargetLabel (EnemyTarget eid) [EnemyEvaded iid eid]
          | eid <- nonEliteEnemies
          ]
        )
    _ -> FloodedSquare <$> runMessage msg attrs
