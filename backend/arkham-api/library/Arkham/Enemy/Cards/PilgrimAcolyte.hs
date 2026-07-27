module Arkham.Enemy.Cards.PilgrimAcolyte (pilgrimAcolyte) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher

newtype PilgrimAcolyte = PilgrimAcolyte EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pilgrimAcolyte :: EnemyCard PilgrimAcolyte
pilgrimAcolyte = enemy PilgrimAcolyte Cards.pilgrimAcolyte

instance HasAbilities PilgrimAcolyte where
  getAbilities (PilgrimAcolyte a) =
    extend1 a $ forcedAbility a 1 $ EnemyAttacks #after Anyone AnyEnemyAttack (be a)

instance RunMessage PilgrimAcolyte where
  runMessage msg e@(PilgrimAcolyte attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    _ -> PilgrimAcolyte <$> liftRunMessage msg attrs
