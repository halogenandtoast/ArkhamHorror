module Arkham.Enemy.Cards.BoaConstrictor (boaConstrictor) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher
import Arkham.Modifier

newtype BoaConstrictor = BoaConstrictor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boaConstrictor :: EnemyCard BoaConstrictor
boaConstrictor = enemy BoaConstrictor Cards.boaConstrictor (4, Static 4, 2) (1, 1)

instance HasAbilities BoaConstrictor where
  getAbilities (BoaConstrictor a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage BoaConstrictor where
  runMessage msg e@(BoaConstrictor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      nextPhaseModifier #upkeep (attrs.ability 1) iid ControlledAssetsCannotReady
      pure e
    _ -> BoaConstrictor <$> liftRunMessage msg attrs
