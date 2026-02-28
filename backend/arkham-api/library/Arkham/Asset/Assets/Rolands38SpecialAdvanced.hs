module Arkham.Asset.Assets.Rolands38SpecialAdvanced (rolands38SpecialAdvanced) where

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
    [controlled_ x 1 $ fightAction $ assetUseCost x Ammo 1]

instance RunMessage Rolands38SpecialAdvanced where
  runMessage msg a@(Rolands38SpecialAdvanced attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers
        sid
        (attrs.ability 1)
        iid
        [ DamageDealt 1
        , CalculatedSkillModifier #combat
            $ IfLocationExistsCalculation (locationWithInvestigator iid <> LocationWithAnyClues) (Fixed 4) (Fixed 2)
        ]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _ -> do
      for_ attrs.controller \iid -> do
        valid <- selectAny $ locationWithInvestigator iid <> LocationWithoutClues
        when valid $ placeTokens (attrs.ability 1) attrs Ammo 1
      pure a
    _ -> Rolands38SpecialAdvanced <$> liftRunMessage msg attrs
