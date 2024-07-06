module Arkham.Asset.Cards.EighteenDerringer (eighteenDerringer, EighteenDerringer (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype EighteenDerringer = EighteenDerringer AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eighteenDerringer :: AssetCard EighteenDerringer
eighteenDerringer = asset EighteenDerringer Cards.eighteenDerringer

instance HasAbilities EighteenDerringer where
  getAbilities (EighteenDerringer attrs) =
    [restrictedAbility attrs 1 ControlsThis $ fightAction $ assetUseCost attrs Ammo 1]

instance RunMessage EighteenDerringer where
  runMessage msg a@(EighteenDerringer attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll [skillTestModifiers source iid [DamageDealt 1, SkillModifier #combat 2], chooseFight]
      pure a
    FailedThisSkillTest _ (isSource attrs -> True) -> do
      push $ AddUses (attrs.ability 1) (toId attrs) Ammo 1
      pure a
    _ -> EighteenDerringer <$> runMessage msg attrs
