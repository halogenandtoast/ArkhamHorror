module Arkham.Asset.Assets.Aquinnah1 (aquinnah1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyAttacks)
import Arkham.Attack.Types
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Enemy.Types (Field (EnemyHealthDamage))
import Arkham.Helpers.Window
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype Aquinnah1 = Aquinnah1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aquinnah1 :: AssetCard Aquinnah1
aquinnah1 = ally Aquinnah1 Cards.aquinnah1 (1, 4)

instance HasAbilities Aquinnah1 where
  getAbilities (Aquinnah1 a) =
    [ controlled
        a
        1
        ( CanDealDamage
            <> oneOf
              [ EnemyCriteria (NotAttackingEnemy <> EnemyExists (EnemyAt YourLocation))
              , exists $ YourLocation <> LocationWithExposableConcealedCard (toSource a)
              ]
        )
        $ triggered (EnemyAttacks #when You AnyEnemyAttack AnyEnemy) (exhaust a <> horrorCost a 1)
    ]

instance RunMessage Aquinnah1 where
  runMessage msg a@(Aquinnah1 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getAttackDetails -> attack) _ -> do
      changeAttackDetails attack.enemy attack {attackDealDamage = False}
      healthDamage' <- field EnemyHealthDamage attack.enemy
      enemies <- select $ enemyAtLocationWith iid <> not_ (be attack.enemy)
      concealed <- getConcealedIds (ForExpose $ toSource attrs) iid
      chooseOneM iid do
        targets enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) healthDamage'
        targets concealed $ exposeConcealed iid (attrs.ability 1)
      pure a
    _ -> Aquinnah1 <$> liftRunMessage msg attrs
