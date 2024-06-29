module Arkham.Asset.Cards.OldShotgun2 (oldShotgun2, OldShotgun2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Fight
import Arkham.Modifier

newtype OldShotgun2 = OldShotgun2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldShotgun2 :: AssetCard OldShotgun2
oldShotgun2 = asset OldShotgun2 Cards.oldShotgun2

instance HasAbilities OldShotgun2 where
  getAbilities (OldShotgun2 a) = [restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage OldShotgun2 where
  runMessage msg a@(OldShotgun2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifiers (attrs.ability 1) iid [SkillModifier #combat 3, NoStandardDamage]
      pushM $ mkChooseFight iid (attrs.ability 1)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      skillTestModifier (attrs.ability 1) iid (DamageDealt $ max 1 $ min 3 n)
      pure a
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      skillTestModifier (attrs.ability 1) iid (DamageDealtToInvestigator $ max 1 $ min 3 n)
      pure a
    _ -> OldShotgun2 <$> liftRunMessage msg attrs
