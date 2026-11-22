module Arkham.Asset.Assets.LukeRobinson (lukeRobinson) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator (canHaveDamageHealed)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Spell))

newtype LukeRobinson = LukeRobinson AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lukeRobinson :: AssetCard LukeRobinson
lukeRobinson = ally LukeRobinson Cards.lukeRobinson (1, 1)

instance HasAbilities LukeRobinson where
  getAbilities (LukeRobinson a) =
    [ restricted a 1 ControlsThis $ forced $ AssetEntersPlay #when (be a)
    , {- "flip Luke Robinson facedown and place 2 of your resources on him, as
      dreams" is the cost, which is what makes the dreaming-card cap gate the
      ability: at the cap 'FlipAssetFacedownCost' is unaffordable and the
      ability is never offered, rather than being paid for and doing nothing. -}
      wantsSkillTest (YourSkillTest $ SkillTestOnCardWithTrait Spell)
        $ limited (MaxPer Cards.lukeRobinson PerRound 1)
        $ controlled a 2 (DuringSkillTest $ SkillTestOnCardWithTrait Spell)
        $ FastAbility
        $ ResourceCost 2
        <> FlipAssetFacedownCost (AssetWithId a.id) 2
    ]

instance RunMessage LukeRobinson where
  runMessage msg a@(LukeRobinson attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      whenM (canHaveDamageHealed (attrs.ability 1) iid) $ healDamage iid (attrs.ability 1) 1
      gainResources iid (attrs.ability 1) 1
      selectOneToHandle iid (attrs.ability 1)
        $ assetControlledBy iid
        <> AssetWithUseType Charge
        <> AssetNotAtUsesX
      pure a
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) (AssetTarget aid) -> do
      placeTokens (attrs.ability 1) aid Charge 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) iid (AnySkillValue 2)
      pure a
    _ -> LukeRobinson <$> liftRunMessage msg attrs
