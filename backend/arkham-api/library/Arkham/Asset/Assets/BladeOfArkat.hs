module Arkham.Asset.Assets.BladeOfArkat (bladeOfArkat) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier

newtype BladeOfArkat = BladeOfArkat AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bladeOfArkat :: AssetCard BladeOfArkat
bladeOfArkat = asset BladeOfArkat Cards.bladeOfArkat

instance HasAbilities BladeOfArkat where
  getAbilities (BladeOfArkat a) =
    [ controlled_ a 1
        $ FastAbility
        $ exhaust a
        <> ResourceCost 1
    , fightAbility a 2 mempty ControlsThis
    ]

instance RunMessage BladeOfArkat where
  runMessage msg a@(BladeOfArkat attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Resource 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let resources = attrs.use Resource
      sid <- getRandom
      skillTestModifier sid (attrs.ability 2) iid (SkillModifier #combat resources)
      chooseFightEnemy sid iid (attrs.ability 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      let resources = min 3 (attrs.use Resource)
      when (resources > 0) do
        chooseAmount iid "Resources to remove" "Resources" 0 resources attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "Resources" -> n) (isTarget attrs -> True) | n > 0 -> do
      removeTokens (attrs.ability 2) (toTarget attrs) Resource n
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 2) iid (DamageDealt n)
      pure a
    _ -> BladeOfArkat <$> liftRunMessage msg attrs
