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
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype FloodedSquare = FloodedSquare LocationAttrs
  deriving anyclass IsLocation
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

instance HasModifiersFor env FloodedSquare

ability :: LocationAttrs -> Ability
ability a = mkAbility a 1 (ActionAbility Nothing $ ActionCost 1)

instance ActionRunner env => HasAbilities env FloodedSquare where
  getAbilities iid NonFast (FloodedSquare attrs) =
    withBaseActions iid NonFast attrs $ do
      counterClockwiseLocation <- getCounterClockwiseLocation (toId attrs)
      nonEliteEnemies <- getSet @EnemyId $ EnemyMatchAll
        [NonEliteEnemy, EnemyAtLocation counterClockwiseLocation]
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
        [NonEliteEnemy, EnemyAtLocation counterClockwiseLocation]
      l <$ push
        (chooseOne
          iid
          [ TargetLabel (EnemyTarget eid) [EnemyEvaded iid eid]
          | eid <- nonEliteEnemies
          ]
        )
    _ -> FloodedSquare <$> runMessage msg attrs
