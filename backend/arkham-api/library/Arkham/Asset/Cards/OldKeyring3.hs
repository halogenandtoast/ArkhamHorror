module Arkham.Asset.Cards.OldKeyring3 (oldKeyring3, OldKeyring3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Helpers.SkillTest (getSkillTestDifficulty, withSkillTest)
import Arkham.Investigate
import Arkham.Modifier

newtype OldKeyring3 = OldKeyring3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldKeyring3 :: AssetCard OldKeyring3
oldKeyring3 = assetWith OldKeyring3 Cards.oldKeyring3 (whenNoUsesL ?~ DiscardWhenNoUses)

instance HasAbilities OldKeyring3 where
  getAbilities (OldKeyring3 attrs) = [investigateAbility attrs 1 mempty ControlsThis]

instance RunMessage OldKeyring3 where
  runMessage msg a@(OldKeyring3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        sid <- getRandom
        skillTestModifier sid (attrs.ability 1) lid (ShroudModifier (-2))
        pushM $ mkInvestigate sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      push $ SpendUses (attrs.ability 1) (toTarget attrs) Key 1

      whenJustM getSkillTestDifficulty \difficulty -> do
        when (difficulty == 0) $ do
          withSkillTest \sid ->
            skillTestModifier sid (attrs.ability 1) iid $ DiscoveredClues 1

      pure a
    _ -> OldKeyring3 <$> liftRunMessage msg attrs
