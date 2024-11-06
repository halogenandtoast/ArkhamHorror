module Arkham.Enemy.Cards.ApostleOfDagon
  ( apostleOfDagon
  , ApostleOfDagon(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ApostleOfDagon = ApostleOfDagon EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

apostleOfDagon :: EnemyCard ApostleOfDagon
apostleOfDagon = enemy ApostleOfDagon Cards.apostleOfDagon (2, Static 3, 2) (1, 1)

instance RunMessage ApostleOfDagon where
  runMessage msg (ApostleOfDagon attrs) = runQueueT $ case msg of
    _ -> ApostleOfDagon <$> liftRunMessage msg attrs
