module Arkham.Asset.Assets.BerettaM19184 (berettaM19184, BerettaM19184 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype BerettaM19184 = BerettaM19184 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

berettaM19184 :: AssetCard BerettaM19184
berettaM19184 = asset BerettaM19184 Cards.berettaM19184

instance HasAbilities BerettaM19184 where
  getAbilities (BerettaM19184 a) =
    [restrictedAbility a 1 ControlsThis $ fightAction $ exhaust a <> assetUseCost a Ammo 1]

instance RunMessage BerettaM19184 where
  runMessage msg a@(BerettaM19184 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid (attrs.ability 1)
      enabled <- skillTestModifiers sid (attrs.ability 1) iid [DamageDealt 1, SkillModifier #combat 4]
      pushAll [enabled, chooseFight]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n >= 2 -> do
      withSkillTest \sid -> do
        additionalDamageDealt <- skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
        if n >= 4
          then pushAll [ready attrs, additionalDamageDealt]
          else do
            player <- getPlayer iid
            push
              $ chooseOne
                player
                [ Label "Ready Beretta M1918" [ready attrs]
                , Label "Deal an additional +1 damage" [additionalDamageDealt]
                ]
      pure a
    _ -> BerettaM19184 <$> runMessage msg attrs
