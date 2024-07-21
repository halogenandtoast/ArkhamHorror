module Arkham.Asset.Cards.BangleOfJinxes1 (bangleOfJinxes1, BangleOfJinxes1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (EnemyAttacks)
import Arkham.Matcher
import Arkham.Prelude

newtype BangleOfJinxes1 = BangleOfJinxes1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bangleOfJinxes1 :: AssetCard BangleOfJinxes1
bangleOfJinxes1 = asset BangleOfJinxes1 Cards.bangleOfJinxes1

instance HasAbilities BangleOfJinxes1 where
  getAbilities (BangleOfJinxes1 a) =
    [ playerLimit PerTestOrAbility
        $ controlledAbility a 1 (DuringSkillTest AnySkillTest)
        $ FastAbility
        $ assetUseCost a Charge 1
    , restrictedAbility a 2 ControlsThis
        $ freeReaction (EnemyAttacks #after You AnyEnemyAttack AnyEnemy)
    ]

instance RunMessage BangleOfJinxes1 where
  runMessage msg a@(BangleOfJinxes1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid attrs iid (AnySkillValue 2)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ AddUses (attrs.ability 2) (toId a) Charge 1
      pure a
    _ -> BangleOfJinxes1 <$> runMessage msg attrs
