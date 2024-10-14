module Arkham.Asset.Assets.Hyperawareness4 (hyperawareness4, Hyperawareness4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype Hyperawareness4 = Hyperawareness4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hyperawareness4 :: AssetCard Hyperawareness4
hyperawareness4 = asset Hyperawareness4 Cards.hyperawareness4

instance HasAbilities Hyperawareness4 where
  getAbilities (Hyperawareness4 a) =
    [ wantsSkillTest (YourSkillTest $ oneOf [#intellect, #agility])
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ OrCost [ResourceCost 1, assetUseCost a #resource 1]
    ]

instance RunMessage Hyperawareness4 where
  runMessage msg a@(Hyperawareness4 attrs) = runQueueT $ case msg of
    Do BeginRound -> pure . Hyperawareness4 $ attrs & tokensL %~ replenish #resource 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        let source = attrs.ability 1
        chooseOneM iid do
          labeled "Choose Intellect" $ skillTestModifier sid source iid (SkillModifier #intellect 1)
          labeled "Choose Agility" $ skillTestModifier sid source iid (SkillModifier #agility 1)
      pure a
    _ -> Hyperawareness4 <$> liftRunMessage msg attrs
