module Arkham.Asset.Assets.ThomasCorriganPast (thomasCorriganPast) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher

newtype ThomasCorriganPast = ThomasCorriganPast AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thomasCorriganPast :: AssetCard ThomasCorriganPast
thomasCorriganPast = ally ThomasCorriganPast Cards.thomasCorriganPast (2, 2)

instance HasAbilities ThomasCorriganPast where
  getAbilities (ThomasCorriganPast a) =
    [ restricted a 1 OnSameLocation
        $ triggered (InitiatedSkillTest #when You AnySkillType AnySkillTestValue #any) (exhaust a)
    ]

instance RunMessage ThomasCorriganPast where
  runMessage msg a@(ThomasCorriganPast attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        onSucceedByEffect sid AnyValue (attrs.ability 1) sid $ drawCards iid (attrs.ability 1) 1
      pure a
    _ -> ThomasCorriganPast <$> liftRunMessage msg attrs
