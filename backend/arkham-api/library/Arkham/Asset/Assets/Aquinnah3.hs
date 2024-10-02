module Arkham.Asset.Assets.Aquinnah3 (Aquinnah3 (..), aquinnah3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Attack.Types
import Arkham.Enemy.Types (Field (EnemyHealthDamage))
import Arkham.Helpers.Window
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype Aquinnah3 = Aquinnah3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aquinnah3 :: AssetCard Aquinnah3
aquinnah3 = ally Aquinnah3 Cards.aquinnah3 (1, 4)

instance HasAbilities Aquinnah3 where
  getAbilities (Aquinnah3 a) =
    [ controlledAbility a 1 (CanDealDamage <> exists (EnemyAt YourLocation))
        $ ReactionAbility
          (Matcher.EnemyAttacks #when You AnyEnemyAttack AnyEnemy)
          (exhaust a <> horrorCost a 1)
    ]

instance RunMessage Aquinnah3 where
  runMessage msg a@(Aquinnah3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getAttackDetails -> attack) _ -> do
      changeAttackDetails attack.enemy attack {attackDealDamage = False}
      healthDamage' <- field EnemyHealthDamage attack.enemy
      enemyIds <- select $ EnemyAt $ locationWithInvestigator iid
      chooseOneM iid $ targets enemyIds $ nonAttackEnemyDamage (attrs.ability 1) healthDamage'
      pure a
    _ -> Aquinnah3 <$> liftRunMessage msg attrs
