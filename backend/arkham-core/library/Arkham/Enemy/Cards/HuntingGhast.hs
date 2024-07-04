module Arkham.Enemy.Cards.HuntingGhast (huntingGhast, HuntingGhast (..)) where

import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Trait (Trait (Gug))

newtype HuntingGhast = HuntingGhast EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingGhast :: EnemyCard HuntingGhast
huntingGhast = enemyWith HuntingGhast Cards.huntingGhast (2, Static 2, 3) (1, 1) (preyL .~ Prey MostDamage)

instance HasAbilities HuntingGhast where
  getAbilities (HuntingGhast attrs) =
    extend
      attrs
      [ mkAbility attrs 1 $ forced $ EnemyEnters #after (LocationWithEnemy (EnemyWithTrait Gug)) (be attrs)
      ]

instance RunMessage HuntingGhast where
  runMessage msg e@(HuntingGhast attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      gugs <- select $ EnemyWithTrait Gug <> ExhaustedEnemy
      for_ gugs $ push . Ready . toTarget
      push $ Msg.EnemyDamage attrs.id $ nonAttack (attrs.ability 1) 1
      pure e
    _ -> HuntingGhast <$> liftRunMessage msg attrs
