module Arkham.Asset.Assets.ForensicKit (forensicKit) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator (canHaveHorrorHealed)
import Arkham.Investigate
import Arkham.Message.Lifted.Choose

newtype ForensicKit = ForensicKit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forensicKit :: AssetCard ForensicKit
forensicKit = asset ForensicKit Cards.forensicKit

instance HasAbilities ForensicKit where
  getAbilities (ForensicKit a) =
    [investigateAbility a 1 (assetUseCost a Supply 1) ControlsThis]

instance RunMessage ForensicKit where
  runMessage msg a@(ForensicKit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigate' <- mkInvestigate sid iid (attrs.ability 1)
      skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
      chooseOneM iid do
        labeled "Use {agility}" $ push $ withSkillType #agility investigate'
        labeled "Use {intellect}" $ push investigate'
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      whenAny (ExhaustedEnemy <> at_ (locationWithInvestigator iid)) do
        canHeal <- canHaveHorrorHealed attrs iid
        when canHeal $ healHorror iid attrs 1
      pure a
    _ -> ForensicKit <$> liftRunMessage msg attrs
