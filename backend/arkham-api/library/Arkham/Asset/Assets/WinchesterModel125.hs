module Arkham.Asset.Assets.WinchesterModel125 (winchesterModel125) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier

newtype WinchesterModel125 = WinchesterModel125 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

winchesterModel125 :: AssetCard WinchesterModel125
winchesterModel125 = asset WinchesterModel125 Cards.winchesterModel125

instance HasAbilities WinchesterModel125 where
  getAbilities (WinchesterModel125 a) =
    [skillTestAbility $ controlled_ a 1 $ fightAction (assetUseCost a Ammo 1)]

instance RunMessage WinchesterModel125 where
  runMessage msg a@(WinchesterModel125 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 3)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n > 0 -> do
      withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 1) iid (DamageDealt $ min 4 n)
      pure a
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n > 0 -> do
      withSkillTest \sid -> do
        skillTestModifiers sid (attrs.ability 1) iid [NoStandardDamage, DamageDealtToInvestigator $ min 5 n]
      pure a
    _ -> WinchesterModel125 <$> liftRunMessage msg attrs
