module Arkham.Asset.Assets.DelilahORourke3 (delilahORourke3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Cost.FieldCost
import Arkham.Enemy.Types qualified as Field
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype DelilahORourke3 = DelilahORourke3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

delilahORourke3 :: AssetCard DelilahORourke3
delilahORourke3 = ally DelilahORourke3 Cards.delilahORourke3 (3, 2)

instance HasModifiersFor DelilahORourke3 where
  getModifiersFor (DelilahORourke3 a) = controllerGets a [SkillModifier #combat 1, SkillModifier #agility 1]

instance HasAbilities DelilahORourke3 where
  getAbilities (DelilahORourke3 a) =
    [ controlled
        a
        1
        ( IfCostsAreIgnored
            $ enemyExists
            $ EnemyAt YourLocation
            <> EnemyWithEvadeValue 0
            <> EnemyCanBeDamagedBySource (a.ability 1)
        )
        ( FastAbility
            $ MaybeFieldResourceCost
              (MaybeFieldCost (EnemyAt YourLocation <> EnemyCanBeDamagedBySource (a.ability 1)) Field.EnemyEvade)
            <> exhaust a
        )
    ]

instance RunMessage DelilahORourke3 where
  runMessage msg a@(DelilahORourke3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalResourcePayment -> n) -> do
      enemies <-
        select (enemyAtLocationWith iid) >>= mapMaybeM \e -> runMaybeT do
          evadeVal <- MaybeT $ field Field.EnemyEvade e
          guard $ evadeVal == n
          pure e
      chooseOneM iid do
        targets enemies \enemy -> do
          exhausted <- matches enemy ExhaustedEnemy
          let x = if exhausted then 2 else 1
          nonAttackEnemyDamage (Just iid) (attrs.ability 1) x enemy
      pure a
    _ -> DelilahORourke3 <$> liftRunMessage msg attrs
