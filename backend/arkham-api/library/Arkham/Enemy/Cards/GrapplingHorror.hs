module Arkham.Enemy.Cards.GrapplingHorror (grapplingHorror) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype GrapplingHorror = GrapplingHorror EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

grapplingHorror :: EnemyCard GrapplingHorror
grapplingHorror =
  enemy GrapplingHorror Cards.grapplingHorror

instance HasModifiersFor GrapplingHorror where
  getModifiersFor (GrapplingHorror a) =
    modifySelect a (investigatorEngagedWith a) [CannotMove]

instance RunMessage GrapplingHorror where
  runMessage msg (GrapplingHorror attrs) =
    GrapplingHorror <$> runMessage msg attrs
