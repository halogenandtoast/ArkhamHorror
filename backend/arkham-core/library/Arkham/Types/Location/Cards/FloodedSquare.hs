module Arkham.Types.Location.Cards.FloodedSquare
  ( floodedSquare
  , FloodedSquare(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher hiding (EnemyEvaded)
import Arkham.Types.Message
import Arkham.Types.Target

newtype FloodedSquare = FloodedSquare LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

floodedSquare :: LocationCard FloodedSquare
floodedSquare = locationWith
  FloodedSquare
  Cards.floodedSquare
  4
  (PerPlayer 1)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasAbilities env FloodedSquare where
  getAbilities iid window (FloodedSquare attrs) =
    withBaseAbilities iid window attrs $ pure
      [ restrictedAbility
          attrs
          1
          (EnemyCriteria $ EnemyExists $ NonEliteEnemy <> EnemyAt
            (LocationInDirection RightOf $ LocationWithId $ toId attrs)
          )
        $ ActionAbility Nothing
        $ ActionCost 1
      | locationRevealed attrs
      ]

instance
  ( HasSet EnemyId env EnemyMatcher
  , LocationRunner env
  )
  => RunMessage env FloodedSquare where
  runMessage msg l@(FloodedSquare attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      counterClockwiseLocation <- getCounterClockwiseLocation (toId attrs)
      nonEliteEnemies <- getSetList @EnemyId $ EnemyMatchAll
        [NonEliteEnemy, EnemyAt $ LocationWithId counterClockwiseLocation]
      l <$ push
        (chooseOne
          iid
          [ TargetLabel (EnemyTarget eid) [EnemyEvaded iid eid]
          | eid <- nonEliteEnemies
          ]
        )
    _ -> FloodedSquare <$> runMessage msg attrs
