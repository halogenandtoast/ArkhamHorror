module Arkham.Enemy.Cards.WilliamBainDefiantToTheLast (williamBainDefiantToTheLast) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype WilliamBainDefiantToTheLast = WilliamBainDefiantToTheLast EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

williamBainDefiantToTheLast :: EnemyCard WilliamBainDefiantToTheLast
williamBainDefiantToTheLast =
  enemy WilliamBainDefiantToTheLast Cards.williamBainDefiantToTheLast (4, Static 4, 4) (0, 1)

instance HasModifiersFor WilliamBainDefiantToTheLast where
  getModifiersFor (WilliamBainDefiantToTheLast a) = do
    modifySelf a [CannotBeDamaged]
    modifySelect a (InvestigatorAt $ locationWithEnemy a) [CannotCommitCards AnyCard]

instance RunMessage WilliamBainDefiantToTheLast where
  runMessage msg (WilliamBainDefiantToTheLast attrs) = WilliamBainDefiantToTheLast <$> runMessage msg attrs
