module Arkham.Asset.Assets.PhysicalTraining4 (physicalTraining4, PhysicalTraining4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype PhysicalTraining4 = PhysicalTraining4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

physicalTraining4 :: AssetCard PhysicalTraining4
physicalTraining4 = asset PhysicalTraining4 Cards.physicalTraining4

instance HasAbilities PhysicalTraining4 where
  getAbilities (PhysicalTraining4 a) =
    [ wantsSkillTest (YourSkillTest $ oneOf [#willpower, #combat])
        $ controlledAbility a 1 DuringAnySkillTest
        $ FastAbility
        $ OrCost [ResourceCost 1, UseCost (be a) #resource 1]
    ]

instance RunMessage PhysicalTraining4 where
  runMessage msg a@(PhysicalTraining4 attrs) = runQueueT $ case msg of
    Do BeginRound -> pure . PhysicalTraining4 $ attrs & tokensL %~ replenish #resource 2
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        let source = attrs.ability 1
        chooseOneM iid do
          labeled "Choose Willpower" $ skillTestModifier sid source iid (SkillModifier #willpower 1)
          labeled "Choose Combat" $ skillTestModifier sid source iid (SkillModifier #combat 1)
      pure a
    _ -> PhysicalTraining4 <$> liftRunMessage msg attrs
