module Arkham.Asset.Assets.ThomasCorriganPresent (thomasCorriganPresent) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype ThomasCorriganPresent = ThomasCorriganPresent AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thomasCorriganPresent :: AssetCard ThomasCorriganPresent
thomasCorriganPresent = ally ThomasCorriganPresent Cards.thomasCorriganPresent (2, 2)

instance HasAbilities ThomasCorriganPresent where
  getAbilities (ThomasCorriganPresent a) =
    [ restricted a 1 (OnSameLocation <> DuringSkillTest (YourSkillTest AnySkillTest))
        $ FastAbility (exhaust a)
    , restricted a 2 OnSameLocation
        $ triggered (InitiatedSkillTest #when You AnySkillType AnySkillTestValue #any) (exhaust a)
    ]

instance RunMessage ThomasCorriganPresent where
  runMessage msg a@(ThomasCorriganPresent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 1) iid (SkillModifier #willpower 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        onSucceedByEffect sid AnyValue (attrs.ability 2) sid $ drawCards iid (attrs.ability 2) 1
      pure a
    _ -> ThomasCorriganPresent <$> liftRunMessage msg attrs
