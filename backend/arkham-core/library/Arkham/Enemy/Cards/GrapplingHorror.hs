module Arkham.Enemy.Cards.GrapplingHorror
  ( GrapplingHorror(..)
  , grapplingHorror
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype GrapplingHorror = GrapplingHorror EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

grapplingHorror :: EnemyCard GrapplingHorror
grapplingHorror =
  enemy GrapplingHorror Cards.grapplingHorror (3, Static 3, 2) (1, 1)

instance HasModifiersFor GrapplingHorror where
  getModifiersFor (InvestigatorTarget iid) (GrapplingHorror a@EnemyAttrs {..})
    = do
      cannotMove <- iid <=~> investigatorEngagedWith enemyId
      pure $ toModifiers a [ CannotMove | cannotMove ]
  getModifiersFor _ _ = pure []

instance RunMessage GrapplingHorror where
  runMessage msg (GrapplingHorror attrs) =
    GrapplingHorror <$> runMessage msg attrs
