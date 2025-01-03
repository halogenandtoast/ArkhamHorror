module Arkham.Asset.Assets.GreenSoapstoneJinxedIdol (greenSoapstoneJinxedIdol) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype GreenSoapstoneJinxedIdol = GreenSoapstoneJinxedIdol AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greenSoapstoneJinxedIdol :: AssetCard GreenSoapstoneJinxedIdol
greenSoapstoneJinxedIdol = asset GreenSoapstoneJinxedIdol Cards.greenSoapstoneJinxedIdol

instance HasAbilities GreenSoapstoneJinxedIdol where
  getAbilities (GreenSoapstoneJinxedIdol a) =
    [ storyControlled_ a 1
        $ ReactionAbility
          (EnemyAttackedSuccessfully #when You AnySource AnyEnemy)
          (assetUseCost a Charge 1 <> exhaust a)
    ]

instance RunMessage GreenSoapstoneJinxedIdol where
  runMessage msg a@(GreenSoapstoneJinxedIdol attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
      pure a
    _ -> GreenSoapstoneJinxedIdol <$> liftRunMessage msg attrs
