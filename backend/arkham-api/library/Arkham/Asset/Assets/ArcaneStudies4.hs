module Arkham.Asset.Assets.ArcaneStudies4 (arcaneStudies4, ArcaneStudies4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype ArcaneStudies4 = ArcaneStudies4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneStudies4 :: AssetCard ArcaneStudies4
arcaneStudies4 = asset ArcaneStudies4 Cards.arcaneStudies4

instance HasAbilities ArcaneStudies4 where
  getAbilities (ArcaneStudies4 a) =
    [ wantsSkillTest (YourSkillTest $ oneOf [#willpower, #intellect])
        $ controlledAbility a 1 (DuringSkillTest AnySkillTest)
        $ FastAbility
        $ OrCost [ResourceCost 1, assetUseCost a #resource 1]
    ]

instance RunMessage ArcaneStudies4 where
  runMessage msg a@(ArcaneStudies4 attrs) = runQueueT $ case msg of
    Do BeginRound -> pure . ArcaneStudies4 $ attrs & tokensL %~ replenish #resource 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        chooseOneM iid do
          labeled "Choose Willpower"
            $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #willpower 1)
          labeled "Choose Intellect"
            $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 1)
      pure a
    _ -> ArcaneStudies4 <$> liftRunMessage msg attrs
