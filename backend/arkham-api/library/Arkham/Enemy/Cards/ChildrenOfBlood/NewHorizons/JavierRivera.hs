module Arkham.Enemy.Cards.ChildrenOfBlood.NewHorizons.JavierRivera (javierRivera) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype JavierRivera = JavierRivera EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

javierRivera :: EnemyCard JavierRivera
javierRivera = enemy JavierRivera Cards.javierRivera

instance RunMessage JavierRivera where
  runMessage msg (JavierRivera attrs) = JavierRivera <$> runMessage msg attrs
