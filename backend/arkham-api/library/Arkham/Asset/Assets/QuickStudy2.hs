module Arkham.Asset.Assets.QuickStudy2 (quickStudy2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype QuickStudy2 = QuickStudy2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickStudy2 :: AssetCard QuickStudy2
quickStudy2 = asset QuickStudy2 Cards.quickStudy2

instance HasAbilities QuickStudy2 where
  getAbilities (QuickStudy2 a) =
    [ wantsSkillTest (YourSkillTest AnySkillTest)
        $ controlled a 1 DuringAnySkillTest
        $ FastAbility (exhaust a <> PlaceClueOnLocationCost 1)
    ]

instance RunMessage QuickStudy2 where
  runMessage msg a@(QuickStudy2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (AnySkillValue 3)
      pure a
    _ -> QuickStudy2 <$> liftRunMessage msg attrs
