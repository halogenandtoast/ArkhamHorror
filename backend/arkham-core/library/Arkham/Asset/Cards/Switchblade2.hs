module Arkham.Asset.Cards.Switchblade2 (Switchblade2 (..), switchblade2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype Switchblade2 = Switchblade2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

switchblade2 :: AssetCard Switchblade2
switchblade2 = asset Switchblade2 Cards.switchblade2

instance HasAbilities Switchblade2 where
  getAbilities (Switchblade2 a) =
    [restrictedAbility a 1 ControlsThis fightAction_]

instance RunMessage Switchblade2 where
  runMessage msg a@(Switchblade2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll
        [ skillTestModifier source iid (SkillModifier #combat 2)
        , chooseFight
        ]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n >= 2 -> do
      push $ skillTestModifier attrs iid (DamageDealt 1)
      pure a
    _ -> Switchblade2 <$> runMessage msg attrs
