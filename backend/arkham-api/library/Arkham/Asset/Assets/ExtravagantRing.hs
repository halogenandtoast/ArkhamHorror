module Arkham.Asset.Assets.ExtravagantRing (extravagantRing) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype ExtravagantRing = ExtravagantRing AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extravagantRing :: AssetCard ExtravagantRing
extravagantRing = asset ExtravagantRing Cards.extravagantRing

instance HasAbilities ExtravagantRing where
  getAbilities (ExtravagantRing a) =
    [ controlled_ a 1
        $ triggered
          (WouldHaveSkillTestResult #when You AnySkillTest #success)
          (assetUseCost a Renown 1 <> exhaust a)
    ]

instance RunMessage ExtravagantRing where
  runMessage msg a@(ExtravagantRing attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        skillTestModifier sid attrs iid (AnySkillValue 2)
        push RecalculateSkillTestResults
      pure a
    _ -> ExtravagantRing <$> liftRunMessage msg attrs
