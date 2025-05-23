module Arkham.Asset.Assets.JennysTwin45s (jennysTwin45s) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Modifier

newtype JennysTwin45s = JennysTwin45s AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jennysTwin45s :: AssetCard JennysTwin45s
jennysTwin45s = asset JennysTwin45s Cards.jennysTwin45s

instance HasAbilities JennysTwin45s where
  getAbilities (JennysTwin45s a) = [restricted a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage JennysTwin45s where
  runMessage msg a@(JennysTwin45s attrs) = runQueueT $ case msg of
    PaidForCardCost _ card payment | toCardId card == toCardId attrs -> do
      let n = totalResourcePayment payment
      JennysTwin45s
        <$> liftRunMessage msg (attrs & printedUsesL .~ Uses Ammo (Fixed n) & tokensL .~ singletonMap Ammo n)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [DamageDealt 1, SkillModifier #combat 2]
      chooseFightEnemy sid iid source
      pure a
    _ -> JennysTwin45s <$> liftRunMessage msg attrs
