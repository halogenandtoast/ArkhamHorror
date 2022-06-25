module Arkham.Enemy.Cards.HasturTheKingInYellow
  ( hasturTheKingInYellow
  , HasturTheKingInYellow(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype HasturTheKingInYellow = HasturTheKingInYellow EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hasturTheKingInYellow :: EnemyCard HasturTheKingInYellow
hasturTheKingInYellow = enemy HasturTheKingInYellow Cards.hasturTheKingInYellow (4, PerPlayer 7, 2) (0, 2)

instance RunMessage HasturTheKingInYellow where
  runMessage msg (HasturTheKingInYellow attrs) =
    HasturTheKingInYellow <$> runMessage msg attrs
