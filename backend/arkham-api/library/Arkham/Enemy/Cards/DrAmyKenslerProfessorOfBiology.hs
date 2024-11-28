module Arkham.Enemy.Cards.DrAmyKenslerProfessorOfBiology
  ( drAmyKenslerProfessorOfBiology
  , DrAmyKenslerProfessorOfBiology(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DrAmyKenslerProfessorOfBiology = DrAmyKenslerProfessorOfBiology EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

drAmyKenslerProfessorOfBiology :: EnemyCard DrAmyKenslerProfessorOfBiology
drAmyKenslerProfessorOfBiology = enemy DrAmyKenslerProfessorOfBiology Cards.drAmyKenslerProfessorOfBiology (0, Static 1, 0) (0, 0)

instance RunMessage DrAmyKenslerProfessorOfBiology where
  runMessage msg (DrAmyKenslerProfessorOfBiology attrs) = runQueueT $ case msg of
    _ -> DrAmyKenslerProfessorOfBiology <$> liftRunMessage msg attrs
