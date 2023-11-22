module Arkham.Asset.Cards.JennysTwin45s (JennysTwin45s (..), jennysTwin45s) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Prelude

newtype JennysTwin45s = JennysTwin45s AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jennysTwin45s :: AssetCard JennysTwin45s
jennysTwin45s = asset JennysTwin45s Cards.jennysTwin45s

instance HasAbilities JennysTwin45s where
  getAbilities (JennysTwin45s a) = [restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage JennysTwin45s where
  runMessage msg a@(JennysTwin45s attrs) = case msg of
    PaidForCardCost _ card payment | toCardId card == toCardId attrs -> do
      let n = totalResourcePayment payment
      JennysTwin45s <$> runMessage msg (attrs & usesL .~ Uses Ammo n)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      pushAll
        [ skillTestModifiers source iid [DamageDealt 1, SkillModifier #combat 2]
        , chooseFightEnemy iid source #combat
        ]
      pure a
    _ -> JennysTwin45s <$> runMessage msg attrs
