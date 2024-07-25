module Arkham.Asset.Cards.OldKeyring (oldKeyring, OldKeyring (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Investigate
import Arkham.Modifier

newtype OldKeyring = OldKeyring AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldKeyring :: AssetCard OldKeyring
oldKeyring = assetWith OldKeyring Cards.oldKeyring (whenNoUsesL ?~ DiscardWhenNoUses)

instance HasAbilities OldKeyring where
  getAbilities (OldKeyring attrs) = [investigateAbility attrs 1 mempty ControlsThis]

instance RunMessage OldKeyring where
  runMessage msg a@(OldKeyring attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        sid <- getRandom
        skillTestModifier sid (attrs.ability 1) lid (ShroudModifier (-2))
        pushM $ mkInvestigate sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      push $ SpendUses (attrs.ability 1) (toTarget attrs) Key 1
      pure a
    _ -> OldKeyring <$> liftRunMessage msg attrs
