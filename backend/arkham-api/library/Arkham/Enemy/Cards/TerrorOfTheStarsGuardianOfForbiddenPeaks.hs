module Arkham.Enemy.Cards.TerrorOfTheStarsGuardianOfForbiddenPeaks (terrorOfTheStarsGuardianOfForbiddenPeaks) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype TerrorOfTheStarsGuardianOfForbiddenPeaks = TerrorOfTheStarsGuardianOfForbiddenPeaks EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

terrorOfTheStarsGuardianOfForbiddenPeaks :: EnemyCard TerrorOfTheStarsGuardianOfForbiddenPeaks
terrorOfTheStarsGuardianOfForbiddenPeaks = enemy TerrorOfTheStarsGuardianOfForbiddenPeaks Cards.terrorOfTheStarsGuardianOfForbiddenPeaks (0, Static 3, 0) (2, 2)

instance RunMessage TerrorOfTheStarsGuardianOfForbiddenPeaks where
  runMessage msg (TerrorOfTheStarsGuardianOfForbiddenPeaks attrs) = runQueueT $ case msg of
    _ -> TerrorOfTheStarsGuardianOfForbiddenPeaks <$> liftRunMessage msg attrs
