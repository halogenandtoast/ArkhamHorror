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
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Matcher hiding (EnemyEvaded)
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window hiding (EnemyEvaded)

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

ability :: LocationAttrs -> Ability
ability a = mkAbility a 1 (ActionAbility Nothing $ ActionCost 1)

instance ActionRunner env => HasAbilities env FloodedSquare where
  getAbilities iid window@(Window Timing.When NonFast) (FloodedSquare attrs) =
    withBaseActions iid window attrs $ do
      counterClockwiseLocation <- getCounterClockwiseLocation (toId attrs)
      nonEliteEnemies <- getSet @EnemyId $ EnemyMatchAll
        [NonEliteEnemy, EnemyAt $ LocationWithId counterClockwiseLocation]
      pure [ ability attrs | notNull nonEliteEnemies ]
  getAbilities iid window (FloodedSquare attrs) = getAbilities iid window attrs

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
