module Arkham.Asset.Assets.HardKnocks4 (hardKnocks4, HardKnocks4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype HardKnocks4 = HardKnocks4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hardKnocks4 :: AssetCard HardKnocks4
hardKnocks4 = asset HardKnocks4 Cards.hardKnocks4

instance HasAbilities HardKnocks4 where
  getAbilities (HardKnocks4 a) =
    [ wantsSkillTest (YourSkillTest $ oneOf [#combat, #agility])
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ OrCost [ResourceCost 1, assetUseCost a #resource 1]
    ]

instance RunMessage HardKnocks4 where
  runMessage msg a@(HardKnocks4 attrs) = runQueueT $ case msg of
    Do BeginRound -> pure . HardKnocks4 $ attrs & tokensL %~ replenish #resource 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        let source = attrs.ability 1
        chooseOneM iid do
          labeled "Choose Combat" $ skillTestModifier sid source iid (SkillModifier #combat 1)
          labeled "Choose Agility" $ skillTestModifier sid source iid (SkillModifier #agility 1)
      pure a
    _ -> HardKnocks4 <$> liftRunMessage msg attrs
