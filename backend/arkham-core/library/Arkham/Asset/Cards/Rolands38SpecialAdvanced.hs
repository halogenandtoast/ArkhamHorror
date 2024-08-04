module Arkham.Asset.Cards.Rolands38SpecialAdvanced (Rolands38SpecialAdvanced (..), rolands38SpecialAdvanced) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Modifier
import Arkham.Token

newtype Rolands38SpecialAdvanced = Rolands38SpecialAdvanced AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rolands38SpecialAdvanced :: AssetCard Rolands38SpecialAdvanced
rolands38SpecialAdvanced = asset Rolands38SpecialAdvanced Cards.rolands38SpecialAdvanced

instance HasAbilities Rolands38SpecialAdvanced where
  getAbilities (Rolands38SpecialAdvanced x) =
    [restrictedAbility x 1 ControlsThis $ fightAction $ assetUseCost x Ammo 1]

instance RunMessage Rolands38SpecialAdvanced where
  runMessage msg a@(Rolands38SpecialAdvanced attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      anyClues <- selectAny $ locationWithInvestigator iid <> LocationWithAnyClues
      let source = attrs.ability 1
      let n = if anyClues then 4 else 2
      sid <- getRandom
      skillTestModifiers sid source iid [DamageDealt 1, SkillModifier #combat n]
      chooseFightEnemy sid iid source
      pure a
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _ -> do
      for_ attrs.controller \iid -> do
        valid <- selectAny $ locationWithInvestigator iid <> LocationWithoutClues
        when valid $ placeTokens (attrs.ability 1) attrs Ammo 1
      pure a
    _ -> Rolands38SpecialAdvanced <$> liftRunMessage msg attrs
