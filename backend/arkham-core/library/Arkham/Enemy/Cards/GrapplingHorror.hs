module Arkham.Enemy.Cards.GrapplingHorror
  ( GrapplingHorror(..)
  , grapplingHorror
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Modifier
import Arkham.Target

newtype GrapplingHorror = GrapplingHorror EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

grapplingHorror :: EnemyCard GrapplingHorror
grapplingHorror =
  enemy GrapplingHorror Cards.grapplingHorror (3, Static 3, 2) (1, 1)

instance HasModifiersFor env GrapplingHorror where
  getModifiersFor _ (InvestigatorTarget iid) (GrapplingHorror a@EnemyAttrs {..})
    = if iid `elem` enemyEngagedInvestigators
      then pure $ toModifiers a [CannotMove]
      else pure []
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage GrapplingHorror where
  runMessage msg (GrapplingHorror attrs) =
    GrapplingHorror <$> runMessage msg attrs
