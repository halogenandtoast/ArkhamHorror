module Arkham.Enemy.Cards.InitiateOfDagon
  ( initiateOfDagon
  , InitiateOfDagon(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype InitiateOfDagon = InitiateOfDagon EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

initiateOfDagon :: EnemyCard InitiateOfDagon
initiateOfDagon = enemy InitiateOfDagon Cards.initiateOfDagon (2, Static 1, 2) (0, 1)

instance RunMessage InitiateOfDagon where
  runMessage msg (InitiateOfDagon attrs) =
    InitiateOfDagon <$> runMessage msg attrs
