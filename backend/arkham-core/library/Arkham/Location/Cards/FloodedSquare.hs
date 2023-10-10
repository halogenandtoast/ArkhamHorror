module Arkham.Location.Cards.FloodedSquare (
  floodedSquare,
  FloodedSquare (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message qualified as Msg
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers

newtype FloodedSquare = FloodedSquare LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

floodedSquare :: LocationCard FloodedSquare
floodedSquare =
  locationWith
    FloodedSquare
    Cards.floodedSquare
    4
    (PerPlayer 1)
    (connectsToL .~ singleton RightOf)

instance HasAbilities FloodedSquare where
  getAbilities (FloodedSquare attrs) =
    withRevealedAbilities attrs
      $ [ restrictedAbility
            attrs
            1
            ( Here
                <> enemyExists
                  ( NonEliteEnemy
                      <> EnemyAt
                        (LocationInDirection RightOf $ LocationWithId $ toId attrs)
                  )
            )
            $ ActionAbility Nothing
            $ ActionCost 1
        ]

instance RunMessage FloodedSquare where
  runMessage msg l@(FloodedSquare attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      mCounterClockwiseLocation <- getCounterClockwiseLocation (toId attrs)
      for_ mCounterClockwiseLocation $ \counterClockwiseLocation -> do
        nonEliteEnemies <- selectList $ NonEliteEnemy <> enemyAt counterClockwiseLocation
        player <- getPlayer iid
        push
          $ chooseOne
            player
            [targetLabel eid [Msg.EnemyEvaded iid eid] | eid <- nonEliteEnemies]
      pure l
    _ -> FloodedSquare <$> runMessage msg attrs
