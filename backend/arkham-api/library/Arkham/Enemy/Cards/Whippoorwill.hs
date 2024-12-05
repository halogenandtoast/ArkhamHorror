module Arkham.Enemy.Cards.Whippoorwill (
  Whippoorwill (..),
  whippoorwill,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype Whippoorwill = Whippoorwill EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

whippoorwill :: EnemyCard Whippoorwill
whippoorwill = enemy Whippoorwill Cards.whippoorwill (2, Static 1, 4) (0, 1)

instance HasModifiersFor Whippoorwill where
  getModifiersFor (Whippoorwill a) = do
    modifySelect a (InvestigatorAt $ locationWithEnemy a) [AnySkillValue (-1)]

instance RunMessage Whippoorwill where
  runMessage msg (Whippoorwill attrs) = Whippoorwill <$> runMessage msg attrs
