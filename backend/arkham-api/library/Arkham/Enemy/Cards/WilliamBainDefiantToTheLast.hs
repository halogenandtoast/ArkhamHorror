module Arkham.Enemy.Cards.WilliamBainDefiantToTheLast (
  williamBainDefiantToTheLast,
  WilliamBainDefiantToTheLast(..),
) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype WilliamBainDefiantToTheLast = WilliamBainDefiantToTheLast EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

williamBainDefiantToTheLast :: EnemyCard WilliamBainDefiantToTheLast
williamBainDefiantToTheLast =
  enemy WilliamBainDefiantToTheLast Cards.williamBainDefiantToTheLast (4, Static 4, 4) (0, 1)

instance HasModifiersFor WilliamBainDefiantToTheLast where
  getModifiersFor (WilliamBainDefiantToTheLast a) = do
    modifySelf a [CannotBeDamaged]
    modifySelect a (InvestigatorAt $ locationWithEnemy a) [CannotCommitCards AnyCard]

instance HasAbilities WilliamBainDefiantToTheLast where
  getAbilities (WilliamBainDefiantToTheLast a) =
    [ mkAbility a 1 $ forced $ RoundEnds #when
    ]

instance RunMessage WilliamBainDefiantToTheLast where
  runMessage msg e@(WilliamBainDefiantToTheLast attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> pure e
    _ -> WilliamBainDefiantToTheLast <$> liftRunMessage msg attrs
