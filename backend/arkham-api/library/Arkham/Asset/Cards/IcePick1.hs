module Arkham.Asset.Cards.IcePick1 (icePick1, IcePick1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype IcePick1 = IcePick1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

icePick1 :: AssetCard IcePick1
icePick1 = asset IcePick1 Cards.icePick1

instance HasAbilities IcePick1 where
  getAbilities (IcePick1 x) =
    [ controlledAbility x 1 (DuringSkillTest $ oneOf [#investigating, #fighting])
        $ FastAbility (exhaust x)
    ]

instance RunMessage IcePick1 where
  runMessage msg a@(IcePick1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
      pure a
    _ -> IcePick1 <$> liftRunMessage msg attrs
