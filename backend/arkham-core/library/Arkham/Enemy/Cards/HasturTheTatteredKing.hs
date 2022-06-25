module Arkham.Enemy.Cards.HasturTheTatteredKing
  ( hasturTheTatteredKing
  , HasturTheTatteredKing(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype HasturTheTatteredKing = HasturTheTatteredKing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hasturTheTatteredKing :: EnemyCard HasturTheTatteredKing
hasturTheTatteredKing = enemy HasturTheTatteredKing Cards.hasturTheTatteredKing (3, PerPlayer 8, 2) (0, 4)

instance RunMessage HasturTheTatteredKing where
  runMessage msg (HasturTheTatteredKing attrs) =
    HasturTheTatteredKing <$> runMessage msg attrs
