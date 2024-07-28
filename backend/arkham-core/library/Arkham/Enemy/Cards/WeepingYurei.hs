module Arkham.Enemy.Cards.WeepingYurei (weepingYurei, WeepingYurei (..)) where

import Arkham.Ability
import Arkham.Attack
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype WeepingYurei = WeepingYurei EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

weepingYurei :: EnemyCard WeepingYurei
weepingYurei =
  enemyWith
    WeepingYurei
    Cards.weepingYurei
    (2, Static 2, 2)
    (0, 2)
    (\a -> a & preyL .~ BearerOf (toId a))

instance HasAbilities WeepingYurei where
  getAbilities (WeepingYurei a) =
    extend
      a
      [ groupLimit PerTestOrAbility
          $ restrictedAbility a 1 (DuringSkillTest $ SkillTestAt $ locationWithEnemy a.id)
          $ forced
          $ RevealChaosToken #after You (IncludeTokenPool $ oneOf [#bless, #curse])
      ]

instance RunMessage WeepingYurei where
  runMessage msg e@(WeepingYurei attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ EnemyAttack $ enemyAttack attrs.id (attrs.ability 1) iid
      pure e
    _ -> WeepingYurei <$> liftRunMessage msg attrs
