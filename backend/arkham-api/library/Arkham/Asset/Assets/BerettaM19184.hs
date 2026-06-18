module Arkham.Asset.Assets.BerettaM19184 (berettaM19184) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.I18n
import Arkham.Message.Lifted.Choose

newtype BerettaM19184 = BerettaM19184 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

berettaM19184 :: AssetCard BerettaM19184
berettaM19184 = asset BerettaM19184 Cards.berettaM19184

instance HasAbilities BerettaM19184 where
  getAbilities (BerettaM19184 a) =
    [controlled_ a 1 $ fightAction $ exhaust a <> assetUseCost a Ammo 1]

instance RunMessage BerettaM19184 where
  runMessage msg a@(BerettaM19184 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFightEnemyWithModifiers sid iid (attrs.ability 1) [DamageDealt 1, SkillModifier #combat 4]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n >= 2 -> do
      if n >= 4
        then do
          readyThis attrs
          modifySkillTest (attrs.ability 1) iid [DamageDealt 1]
        else do
          chooseOneM iid $ cardI18n do
            labeled' "berettaM19184.readyBerettaM1918" $ readyThis attrs
            labeled' "berettaM19184.dealAnAdditional1Damage" do
              modifySkillTest (attrs.ability 1) iid [DamageDealt 1]
      pure a
    _ -> BerettaM19184 <$> liftRunMessage msg attrs
