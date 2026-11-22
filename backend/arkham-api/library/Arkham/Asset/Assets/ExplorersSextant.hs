module Arkham.Asset.Assets.ExplorersSextant (explorersSextant) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Dreaming.Lifted (flipAnAssetFacedown)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.SkillTest (getSkillTestTargetedLocation, withSkillTest)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype ExplorersSextant = ExplorersSextant AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

explorersSextant :: AssetCard ExplorersSextant
explorersSextant = asset ExplorersSextant Cards.explorersSextant

instance HasAbilities ExplorersSextant where
  getAbilities (ExplorersSextant a) = [investigateAbility a 1 mempty ControlsThis]

instance RunMessage ExplorersSextant where
  runMessage msg a@(ExplorersSextant attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 1)
      investigate sid iid (attrs.ability 1)
      pure a
    -- Mandatory on failure, not a cost: the Sextant is itself a legal target.
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      flipAnAssetFacedown iid 3
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      when attrs.ready $ withLocationOf iid \loc -> do
        mTargetLoc <- getSkillTestTargetedLocation
        priority $ chooseOneM iid do
          (cardI18n $ labeled "explorersSextant.exhaustForClue") do
            exhaustThis attrs
            -- "at your location", which is not necessarily where the
            -- investigation happened.
            withSkillTest \sid ->
              skillTestModifier sid (attrs.ability 1) iid
                $ if mTargetLoc == Just loc then DiscoveredClues 1 else DiscoveredCluesAt loc 1
          labeledI "doNotExhaust" nothing
      pure a
    _ -> ExplorersSextant <$> liftRunMessage msg attrs
