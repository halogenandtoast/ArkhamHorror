module Arkham.Asset.Assets.AzureFlame3 (azureFlame3, AzureFlame3 (..)) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosToken
import Arkham.Fight
import Arkham.Matcher
import Arkham.Modifier

newtype AzureFlame3 = AzureFlame3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azureFlame3 :: AssetCard AzureFlame3
azureFlame3 = asset AzureFlame3 Cards.azureFlame3

instance HasAbilities AzureFlame3 where
  getAbilities (AzureFlame3 a) =
    [ restricted a 1 ControlsThis
        $ ActionAbilityWithSkill [#fight] #willpower
        $ Costs [ActionCost 1, assetUseCost a Charge 1]
    ]

instance RunMessage AzureFlame3 where
  runMessage msg a@(AzureFlame3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid attrs iid [DamageDealt 1, SkillModifier #willpower 2]
      onRevealChaosTokenEffect sid (mapOneOf ChaosTokenFaceIs [ElderSign, PlusOne, Zero]) source sid do
        assignDamage iid source 1
      aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      pure a
    _ -> AzureFlame3 <$> liftRunMessage msg attrs
