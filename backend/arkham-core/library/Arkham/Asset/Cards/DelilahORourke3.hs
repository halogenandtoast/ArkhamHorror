module Arkham.Asset.Cards.DelilahORourke3 (
  delilahORourke3,
  DelilahORourke3 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost.FieldCost
import Arkham.DamageEffect
import Arkham.Enemy.Types qualified as Field
import Arkham.Matcher
import Arkham.Projection

newtype DelilahORourke3 = DelilahORourke3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

delilahORourke3 :: AssetCard DelilahORourke3
delilahORourke3 = ally DelilahORourke3 Cards.delilahORourke3 (3, 2)

instance HasModifiersFor DelilahORourke3 where
  getModifiersFor (InvestigatorTarget iid) (DelilahORourke3 a) | a `controlledBy` iid = do
    pure $ toModifiers a [SkillModifier #combat 1, SkillModifier #agility 1]
  getModifiersFor _ _ = pure []

instance HasAbilities DelilahORourke3 where
  getAbilities (DelilahORourke3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ FastAbility
        $ MaybeFieldResourceCost
          (MaybeFieldCost (EnemyAt YourLocation) Field.EnemyEvade)
        <> exhaust a
    ]

instance RunMessage DelilahORourke3 where
  runMessage msg a@(DelilahORourke3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalResourcePayment -> n) -> do
      enemies <-
        selectList (enemyAtLocationWith iid) >>= mapMaybeM \e -> do
          exhausted <- e <=~> ExhaustedEnemy
          runMaybeT $ do
            evadeVal <- MaybeT $ field Field.EnemyEvade e
            guard (evadeVal == n)
            pure (e, exhausted)
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [ targetLabel enemy [EnemyDamage enemy $ nonAttack (toAbilitySource attrs 1) x]
          | (enemy, exhausted) <- enemies
          , let x = if exhausted then 2 else 1
          ]
      pure a
    _ -> DelilahORourke3 <$> runMessage msg attrs
