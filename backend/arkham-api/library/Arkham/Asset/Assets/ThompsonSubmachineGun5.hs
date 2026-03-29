module Arkham.Asset.Assets.ThompsonSubmachineGun5 (thompsonSubmachineGun5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Modifier

newtype ThompsonSubmachineGun5 = ThompsonSubmachineGun5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thompsonSubmachineGun5 :: AssetCard ThompsonSubmachineGun5
thompsonSubmachineGun5 = asset ThompsonSubmachineGun5 Cards.thompsonSubmachineGun5

instance HasAbilities ThompsonSubmachineGun5 where
  getAbilities (ThompsonSubmachineGun5 a) =
    [ skillTestAbility $ controlled_ a 1 $ fightAction (assetUseCost a Ammo 1)
    , skillTestAbility
        $ controlled_ a 2
        $ triggeredAction #fight (ActivateAbility #after You (AbilityIs (toSource a) 1)) (assetUseCost a Ammo 1)
    ]

instance RunMessage ThompsonSubmachineGun5 where
  runMessage msg a@(ThompsonSubmachineGun5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #combat 3, DamageDealt 1]
      chooseFightEnemy sid iid source
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      sid <- getRandom
      skillTestModifier sid source iid (DamageDealt 1)
      chooseFightEnemy sid iid source
      pure a
    _ -> ThompsonSubmachineGun5 <$> liftRunMessage msg attrs
