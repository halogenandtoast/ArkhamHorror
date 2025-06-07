module Arkham.Enemy.Cards.BrotherhoodCultist (brotherhoodCultist) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types qualified as Field
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Projection

newtype BrotherhoodCultist = BrotherhoodCultist EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brotherhoodCultist :: EnemyCard BrotherhoodCultist
brotherhoodCultist = enemy BrotherhoodCultist Cards.brotherhoodCultist (2, Static 3, 2) (0, 1)

instance HasModifiersFor BrotherhoodCultist where
  getModifiersFor (BrotherhoodCultist a) = do
    doom <- field Field.EnemyDoom (toId a)
    modifySelfWhen a (doom > 0) [EnemyFight doom, EnemyEvade doom]

instance HasAbilities BrotherhoodCultist where
  getAbilities (BrotherhoodCultist a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacked #when You AnySource $ be a

instance RunMessage BrotherhoodCultist where
  runMessage msg e@(BrotherhoodCultist attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom attrs attrs 1
      pure e
    _ -> BrotherhoodCultist <$> liftRunMessage msg attrs
