module Arkham.Asset.Assets.MaryZielinskiPresent (maryZielinskiPresent) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Modifier

newtype MaryZielinskiPresent = MaryZielinskiPresent AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maryZielinskiPresent :: AssetCard MaryZielinskiPresent
maryZielinskiPresent = ally MaryZielinskiPresent Cards.maryZielinskiPresent (2, 2)

instance HasAbilities MaryZielinskiPresent where
  getAbilities (MaryZielinskiPresent a) =
    [ restricted a 1 (OnSameLocation <> DuringSkillTest (YourSkillTest AnySkillTest))
        $ FastAbility (exhaust a)
    , investigateAbility a 2 (exhaust a) OnSameLocation
    ]

instance RunMessage MaryZielinskiPresent where
  runMessage msg a@(MaryZielinskiPresent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      pushM $ mkInvestigate sid iid (attrs.ability 2) <&> setTarget attrs
      pure a
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      gainClues iid (attrs.ability 2) 1
      pure a
    _ -> MaryZielinskiPresent <$> liftRunMessage msg attrs
