module Arkham.Asset.Cards.LabCoat1 (labCoat1, LabCoat1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher

newtype LabCoat1 = LabCoat1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

labCoat1 :: AssetCard LabCoat1
labCoat1 = assetWith LabCoat1 Cards.labCoat1 $ (healthL ?~ 1) . (sanityL ?~ 1)

instance HasAbilities LabCoat1 where
  getAbilities (LabCoat1 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (WouldHaveSkillTestResult #when You (SkillTestOnCard #seeker) (FailureResult $ atMost 1))
          (exhaust a)
    ]

instance RunMessage LabCoat1 where
  runMessage msg a@(LabCoat1 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ PassSkillTestBy 0
      pure a
    _ -> LabCoat1 <$> liftRunMessage msg attrs
