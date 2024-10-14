module Arkham.Asset.Assets.DigDeep4 (digDeep4, DigDeep4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype DigDeep4 = DigDeep4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

digDeep4 :: AssetCard DigDeep4
digDeep4 = asset DigDeep4 Cards.digDeep4

instance HasAbilities DigDeep4 where
  getAbilities (DigDeep4 a) =
    [ wantsSkillTest (YourSkillTest $ oneOf [#willpower, #agility])
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ OrCost [ResourceCost 1, assetUseCost a #resource 1]
    ]

instance RunMessage DigDeep4 where
  runMessage msg a@(DigDeep4 attrs) = runQueueT $ case msg of
    Do BeginRound -> pure . DigDeep4 $ attrs & tokensL %~ replenish #resource 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        let source = attrs.ability 1
        chooseOneM iid do
          labeled "Choose Willpower" $ skillTestModifier sid source iid (SkillModifier #willpower 1)
          labeled "Choose Agility" $ skillTestModifier sid source iid (SkillModifier #agility 1)
      pure a
    _ -> DigDeep4 <$> liftRunMessage msg attrs
