module Arkham.Asset.Cards.Suggestion1 (suggestion1, Suggestion1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Evade
import Arkham.Prelude

newtype Suggestion1 = Suggestion1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suggestion1 :: AssetCard Suggestion1
suggestion1 = assetWith Suggestion1 Cards.suggestion1 (whenNoUsesL ?~ DiscardWhenNoUses)

instance HasAbilities Suggestion1 where
  getAbilities (Suggestion1 a) =
    [evadeAbility a 1 (ActionCost 1 <> exhaust a) ControlsThis]

instance RunMessage Suggestion1 where
  runMessage msg a@(Suggestion1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      sid <- getRandom
      chooseEvade <- toMessage <$> mkChooseEvade sid iid source
      pushAll [skillTestModifier sid source iid (AddSkillValue #willpower), chooseEvade]
      pure a
    PassedThisSkillTestBy _ (isSource attrs -> True) n | n < 2 -> do
      push $ SpendUses (attrs.ability 1) (toTarget attrs) Charge 1
      pure a
    FailedThisSkillTest _ (isSource attrs -> True) -> do
      push $ SpendUses (attrs.ability 1) (toTarget attrs) Charge 1
      pure a
    _ -> Suggestion1 <$> runMessage msg attrs
