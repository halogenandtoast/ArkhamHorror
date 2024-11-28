module Arkham.Enemy.Cards.ProfessorWilliamDyerProfessorOfGeology
  ( professorWilliamDyerProfessorOfGeology
  , ProfessorWilliamDyerProfessorOfGeology(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ProfessorWilliamDyerProfessorOfGeology = ProfessorWilliamDyerProfessorOfGeology EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

professorWilliamDyerProfessorOfGeology :: EnemyCard ProfessorWilliamDyerProfessorOfGeology
professorWilliamDyerProfessorOfGeology = enemy ProfessorWilliamDyerProfessorOfGeology Cards.professorWilliamDyerProfessorOfGeology (0, Static 1, 0) (0, 0)

instance RunMessage ProfessorWilliamDyerProfessorOfGeology where
  runMessage msg (ProfessorWilliamDyerProfessorOfGeology attrs) = runQueueT $ case msg of
    _ -> ProfessorWilliamDyerProfessorOfGeology <$> liftRunMessage msg attrs
