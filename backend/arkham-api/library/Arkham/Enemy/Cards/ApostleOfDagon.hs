module Arkham.Enemy.Cards.ApostleOfDagon (apostleOfDagon, ApostleOfDagon (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Key
import Arkham.Matcher

newtype ApostleOfDagon = ApostleOfDagon EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

apostleOfDagon :: EnemyCard ApostleOfDagon
apostleOfDagon = enemy ApostleOfDagon Cards.apostleOfDagon (2, Static 3, 2) (1, 1)

instance HasAbilities ApostleOfDagon where
  getAbilities (ApostleOfDagon a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemySpawns #after Anywhere $ be a

instance RunMessage ApostleOfDagon where
  runMessage msg e@(ApostleOfDagon attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeKey attrs BlackKey
      pure e
    _ -> ApostleOfDagon <$> liftRunMessage msg attrs
