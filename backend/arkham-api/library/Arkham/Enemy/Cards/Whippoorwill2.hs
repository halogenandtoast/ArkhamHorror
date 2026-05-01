module Arkham.Enemy.Cards.Whippoorwill2 (whippoorwill2) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype Whippoorwill2 = Whippoorwill2 EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

whippoorwill2 :: EnemyCard Whippoorwill2
whippoorwill2 = enemy Whippoorwill2 Cards.whippoorwill2 (2, Static 1, 4) (0, 1)

instance HasModifiersFor Whippoorwill2 where
  getModifiersFor (Whippoorwill2 a) = do
    modifySelect a (InvestigatorAt $ locationWithEnemy a) [AnySkillValue (-1)]
