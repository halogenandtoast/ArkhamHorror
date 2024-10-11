module Arkham.Asset.Assets.IcePick3 (icePick3, IcePick3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype IcePick3 = IcePick3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

icePick3 :: AssetCard IcePick3
icePick3 = asset IcePick3 Cards.icePick3

instance HasAbilities IcePick3 where
  getAbilities (IcePick3 x) =
    [ controlledAbility x 1 (DuringSkillTest $ oneOf [#investigating, #fighting])
        $ FastAbility (exhaust x)
    ]

instance RunMessage IcePick3 where
  runMessage msg a@(IcePick3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
      push $ AddSubscriber (toTarget attrs)
      pure a
    PassedSkillTest iid (Just action) _ (isTarget attrs -> True) _ _ | Just iid == attrs.controller -> do
      withSkillTest \sid -> do
        when (action == #fight) do
          chooseOneM iid do
            labeled "Discard Ice Pick (3) to do +1 damage" do
              toDiscardBy iid (attrs.ability 1) attrs
              skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
            labeled "Do not Discard" nothing
        when (action == #investigate) do
          chooseOneM iid do
            labeled "Discard Ice Pick (3) to discover 1 additional clue" do
              toDiscardBy iid (attrs.ability 1) attrs
              skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
            labeled "Do not Discard" nothing
      pure a
    _ -> IcePick3 <$> liftRunMessage msg attrs
